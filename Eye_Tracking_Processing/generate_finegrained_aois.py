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
        
        if (bar['id'] in relevant_indices):
            aois.append({'relevant': bar_coord})
        elif (bar['id'] == 'legend'):
            aois.append({'legend': bar_coord})
        else:    
            aois.append({'non-relevant': bar_coord})
            
    fout = open('finegrained_aois_adjusted/{}.aoi'.format(msnv), 'w')
    
    for aoi in aois:
        aoi_name = next(iter(aoi))
        
        aoi_coord = aoi[aoi_name]
        
        fout.write(str(aoi_name))
        for i in range(len(aoi_coord)):
            string = '\t' + str(aoi_coord[i][0]) + ',' + str(aoi_coord[i][1])
            fout.write(string)
                
        fout.write('\n')
    
    fout.close()
            

