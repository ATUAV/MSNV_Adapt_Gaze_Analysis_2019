'''
Scirpt for generating non-relevant bar AOIs. 
In each .aoi file generated for a msnv, all lines except the last correspond to 
1 mark or the legend, with the aoi name set to its id. The
last line simply gives the semi-colon seperated concatenation of all the above 
AOIs' coordinates (excl. the legend), and does not actually define an AOI we want.
'''
import pandas as pd
import json


intervention_df = pd.read_csv("intervention.csv")
intervention_df['msnv'] = intervention_df['name'].str.split('_', 1).str[0]
intervention_grouped = intervention_df.groupby('msnv')

msnv_ids = [27, 60, 11, 30, 62, 72, 28, 74, 5, 20, 76, 66, 9, 3]
#msnv_ids = [30]
offsets = (604,80)

for msnv, interventions in intervention_grouped:
    aois = []
    relevant_indices = []
    if (int(msnv) not in msnv_ids):
        continue
    
    with open('data_updated/{}_updated.json'.format(msnv)) as f:
        file = json.load(f)
        marks = file['marks']['marks']
        #print(len(marks))
    
        for index, intervention in interventions.iterrows():
            arguments = json.loads(intervention['arguments'])
            id = arguments["id"]
            relevant_indices.append(id)
                           
    for bar in marks:
        top_left_x = offsets[0] + bar['left'] - 1
        top_left_y = offsets[1] + bar['top'] - 1

        top_right_x = top_left_x + bar['width'] + 2
        top_right_y = top_left_y

        bottom_left_x = top_left_x
        bottom_left_y = top_left_y + bar['height'] + 2

        bottom_right_x = top_right_x
        bottom_right_y = bottom_left_y
            
        bar_coord = [(top_left_x, top_left_y), (top_right_x, top_right_y), 
                         (bottom_right_x, bottom_right_y), (bottom_left_x, bottom_left_y)]
        
        aois.append({bar['id']: bar_coord})
            
    f_nr = open('non_relevant_aois_adjusted/{}_NR.aoi'.format(msnv), 'w')
    f_rel =  open('relevant_aois_adjusted/{}_Relevant.aoi'.format(msnv), 'w')
    rel_overall_list = non_rel_overall_list = 'overall_list'
    
    for aoi in aois:
        aoi_name = next(iter(aoi))
        if aoi_name in relevant_indices:
            fout = f_rel
        else: 
            fout = f_nr
        
        aoi_coord = aoi[aoi_name]
        
        fout.write(str(aoi_name))
        for i in range(len(aoi_coord)):
            string = '\t' + str(aoi_coord[i][0]) + ',' + str(aoi_coord[i][1])
            fout.write(string)
            if (aoi_name != 'legend'):
                if aoi_name in relevant_indices:
                    rel_overall_list += string
                    if (i == 3):
                        rel_overall_list += ';'
                else: 
                    non_rel_overall_list += string
                    if (i == 3):
                        non_rel_overall_list += ';'
                
        fout.write('\n')
    
    f_rel.write(rel_overall_list)
    f_nr.write(non_rel_overall_list)
    f_rel.close()
    f_nr.close()
            