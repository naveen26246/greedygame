import json

line_no =0
col_dict = {}
total_cols =0
with open('data_small') as fh:
    for line in fh:
        line_no += 1
#        print(line)
        jo = json.loads(line)
        for jo_key in jo.keys():
            print(jo_key)
            if jo_key not in col_names.keys():
                col_dict[jo_key]  = {}
                #print(jo_key,len(col_names.keys()))
            
            for inner_key in (jo[jo_key]).keys():
                if inner_key not in col_dict[jo_key].keys():
                    col_dict[jo_key][inner_key] = len(col_dict[jo_key].keys()) - 1
                print(inner_key)
        
              
        
