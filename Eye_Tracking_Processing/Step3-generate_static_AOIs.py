import pandas as pd
import json


rule_intervention_df = pd.read_csv("rule_intervention_payload.csv")
rule_payload = rule_intervention_df.groupby('rule_name')
rules_ignored = ["5_ref_109_rule", "5_ref_110_rule", "5_ref_106_rule", "30_ref_102_rule", "62_ref_102_rule"]

intervention_df = pd.read_csv("intervention.csv")

msnv_ids = [27, 60, 11, 30, 62, 72, 28, 74, 5, 20, 76, 66, 9, 3]
#msnv_ids = [30]
offsets = {27: (604,80), 60: (604,80), 11: (604,80), 30: (604,80), 62: (604,80), 72: (604,80), 
           28: (604,80), 74: (604,80), 5: (604,80), 20: (604,80), 76: (604,80), 66: (604,80), 9: (604,80), 3: (604,80)}
msnv_aois = {}
for msnv_id in msnv_ids:
    msnv_aois[msnv_id] = []


for rule, interventions in rule_payload:
    # get all AIOs for each intervention rule and combine them

    if (rule in rules_ignored):
        continue
    msnv = rule.split("_")[0]
    if (int(msnv) not in msnv_ids):
        continue
    print(rule)
    rule_coord = []
    
    with open('data_updated/{}_updated.json'.format(msnv)) as f:
        file = json.load(f)
        marks = file['marks']['marks']
        
        for index, intervention in interventions.iterrows():
            # print(intervention["intervention_name"])
            arguments = intervention_df[intervention_df.name == intervention['intervention_name']].iloc[0]['arguments']
            arguments = json.loads(arguments)
            
            id = arguments["id"]
            bar = next((x for x in marks if x['id'] == id), None)
            if (bar == None): 
                print('cannot find bar with id {} in msnv {}'.format(id, msnv))
            print(id, bar['id'])
            
            top_left_x = offsets[int(msnv)][0] + bar['left'] - 1
            top_left_y = offsets[int(msnv)][1] + bar['top'] - 1

            top_right_x = top_left_x + bar['width'] + 2
            top_right_y = top_left_y

            bottom_left_x = top_left_x
            bottom_left_y = top_left_y + bar['height'] + 2

            bottom_right_x = top_right_x
            bottom_right_y = bottom_left_y
            
            bar_coord = [(top_left_x, top_left_y), (top_right_x, top_right_y), 
                         (bottom_right_x, bottom_right_y), (bottom_left_x, bottom_left_y), (top_left_x, top_left_y)]
            
            rule_coord.extend(bar_coord)
    
    msnv_aois[int(msnv)].append({rule: rule_coord})
 
    
for msnv, aois in msnv_aois.items():
    with open('static_aois_adjusted/{}_intervention.aoi'.format(msnv), 'w') as fout:
        for aoi in aois:
            aoi_name = next(iter(aoi))
            aoi_coord = aoi[aoi_name]
            
            fout.write(aoi_name)
            for vertex in aoi_coord:
                fout.write('\t' + str(vertex[0]) + ',' + str(vertex[1]))
            fout.write('\n')
    
            
            
            
            
            
