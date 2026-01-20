sampler.toml,
Why do I still see, 

qc_row = "H"
samples_per_plate = 84
qc_plate = "B"

remove those! never used!


the @src/qg/config_models_samplers.py are to check the tomls.                                     
                                                                                                    
  I still see a lot of shitty code like sample_rows: list[str] | None = None                        
      qc_row: str | None = None

      cols: list[int] | None = None
      samples_per_plate: int | None = None
      # Container configs
      vial: GridContainer | None = None
      plate: GridContainer | None = None
                                                                                                    
                                                                                                    
  or     fill_order: Literal["row_major"] | None = None
      position_format: str | None = None
      # These may be inherited from parent or defined here
      sample_rows: list[str] | None = None
      qc_row: str | None = None
      cols: list[int] | None = None
      samples_per_plate: int | None = None
                        
  or     Vanquish: GridSampler | None = None
      MClass48: GridSampler | None = None
      Evosep: EvosepSampler | None = None
                        
- Eeach of the samplers is very differen. Therefore, I would not try to generalize,
- we have 9 [] sections in toml, just give me 9 model classes to read the config. then we will initialize 6 samplers 3x2,
<Evosep/Mclass/Canquish>_<vial/plate>, each will implement the same interface.
- but again, python is not OO, so by interface I mean same set of functions
no inheritance.
- we can have a factory -> input e.g. Mclass48 and Mclass48.vial pydantic classes outputs Mclass_vial instance implementing a sampler interface. etc and so forth.
- if there is no such section in the python skill you must add -> no inheritance, but ducktyping and composition!

