import glob
import os

SLD_FILES = glob_wildcards("sldfiles/{sample}.sld").sample

rule all:
    input:
        expand("csvfiles/{sample}.csv", sample=SLD_FILES)

rule convert_sld_to_csv:
    input:
        "sldfiles/{sample}.sld"
    output:
        "csvfiles/{sample}.csv"
    shell:
        "python3 parse_sld.py {input} --csv {output}"
