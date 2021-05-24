import os
import sys

if len(sys.argv) > 1:
    generator_version = sys.argv[1]
else:
    generator_version = "4.0.0"

cmd = f"docker run --rm openapitools/openapi-generator-cli:v{generator_version} list > languages.txt"
os.system(cmd)

with open("languages.txt") as f:
    lines = [
        line.strip()
        .replace("- ", "")
        .replace(" (beta)", "")
        .replace(" (experimental)", "")
        .replace(" (deprecated)", "")
        for line in f
    ]

lines = list(filter(None, lines))
language_list = lines[
    lines.index("CLIENT generators:") + 1 : lines.index("SERVER generators:")
]

textfile = open("languages.txt", "w")
for language in language_list:
    textfile.write(language + "\n")

textfile.close()
