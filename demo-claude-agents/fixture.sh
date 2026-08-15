#!/usr/bin/env bash
# Builds the demo fixture: a small bleep repo with enough real compilation work
# (30 circe-derived case classes) that a cold worktree compile visibly costs
# seconds while a copy-state-seeded one is a no-op. Usage: fixture.sh <target-dir>
set -euo pipefail

[ $# -eq 1 ] || { echo "usage: fixture.sh <target-dir>" >&2; exit 1; }
target="$1"
[ -e "$target" ] && { echo "refusing to overwrite existing $target" >&2; exit 1; }

# pin $version to the installed binary so the demo records no version-switch noise
version=$(bleep --no-color --help | grep -oE '\(version [^)]+\)' | sed 's/(version //; s/)//')
[ -n "$version" ] || { echo "could not detect bleep version" >&2; exit 1; }

mkdir -p "$target"/core/src/scala/com/example "$target"/core-test/src/scala/com/example
cd "$target"

cat > bleep.yaml <<YAML
\$schema: https://raw.githubusercontent.com/oyvindberg/bleep/master/schema.json
\$version: $version
jvm:
  name: graalvm-community:25.0.1
projects:
  core:
    dependencies: io.circe::circe-generic:0.14.15
    extends: template-common
  core-test:
    dependencies: org.scalameta::munit:1.3.4
    dependsOn: core
    extends: template-common
    isTestProject: true
templates:
  template-common:
    platform:
      name: jvm
    scala:
      version: 3.8.4
YAML

for i in $(seq 1 30); do
  printf 'package com.example\n\nimport io.circe.Codec\n\ncase class Record%d(id: Int, name: String, tags: List[String] = Nil) derives Codec:\n  def describe: String = s"Record%d($id, $name)"\n  def withTag(t: String): Record%d = copy(tags = t :: tags)\n\nobject Record%d:\n  def sample: Record%d = Record%d(%d, "sample").withTag("t%d")\n' \
    "$i" "$i" "$i" "$i" "$i" "$i" "$i" "$i" > core/src/scala/com/example/Record$i.scala
done

cat > core-test/src/scala/com/example/RecordTest.scala <<'SCALA'
package com.example

class RecordTest extends munit.FunSuite:
  test("withTag prepends") {
    assertEquals(Record1(1, "a").withTag("x").tags, List("x"))
  }
SCALA

git init -q
git add -A
git -c user.email=demo@bleep.build -c user.name=demo commit -qm 'demo fixture'
bleep compile --no-tui --no-color
echo "fixture ready at $target"
