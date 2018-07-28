
import pandas as pd
import numpy as np

if __name__ =="__main__":
    
    df_bank = pd.read_csv("bank-additional/bank-additional/bank-additional-full.csv" )
    
    #mapping categorigal values   
    #Jobs
    jobs_categories = {0:'admin.',1:'blue-collar',2:'entrepreneur',3:'housemaid',4:'management',5:'retired',6:'self-employed',7:'services',8:'student',9:'technician',10:'unemployed',11:'unknown_job'}
    dummies_df = pd.get_dummies(df_bank['job'].replace(jobs_categories))   
    df_bank = df_bank.join(dummies_df,lsuffix = 'job_')
    df_bank = df_bank.drop(columns = ['job'])

    #marriage
    marriage_categories = {0:'single',1:'married',2:'divorced'}
    dummies_marr = pd.get_dummies(df_bank['marital'].replace(marriage_categories)) 
    #dummies_marr.columns[3] = dummies_marr.columns[3] + 'marriage'
    df_bank = df_bank.join(dummies_marr,rsuffix = '_marriage')
    df_bank = df_bank.drop(columns = ['marital'])
    
    #housing
    housing_categories = {0:'no',1:'yes',2:'unknown'}
    dummies_housing = pd.get_dummies(df_bank['housing'].replace(housing_categories))
    df_bank = df_bank.join(dummies_housing,rsuffix = 'housing_')
    df_bank = df_bank.drop(columns = ['housing'])
    
    loan_categories = {0:'no',1:'yes',2:'unknown'}
    dummies_loan = pd.get_dummies(df_bank['loan'].replace(housing_categories))
    df_bank = df_bank.join(dummies_loan,rsuffix = 'loan_')
    df_bank = df_bank.drop(columns = ['loan']) 
    
    df_bank['contact'] = np.where(df_bank['contact' ] == 'telephone',1,0 )
        
    
    months = {0:'jan',1:'feb',2:'mar',3:'apr',4:'may',5:'jun',6:'jul',7:'aug',8:'sep',9:'oct',10:'nov',11:'dec'}
    dummies_months = pd.get_dummies(df_bank['month'].replace(months))
    df_bank = df_bank.join(dummies_months)
    df_bank = df_bank.drop(columns = ['month'])

    
    week = {0:'mon',1:'tue',2:'wed',3:'thu',4:'fri'}
    dummies_week = pd.get_dummies(df_bank['day_of_week'].replace(months))
    df_bank = df_bank.join(dummies_week)
    df_bank = df_bank.drop(columns = ['day_of_week'])
    
    #poutcome
    poutcome_categories = {0:'failure.',1:'nonexistent',2:'success'}
    dummies_poutcome = pd.get_dummies(df_bank['poutcome'].replace(poutcome_categories))   
    df_bank = df_bank.join(dummies_poutcome,rsuffix = '_poutcome')
    df_bank = df_bank.drop(columns = ['poutcome'])
    
    #education
    df_bank = df_bank.replace({'basic.4y':'basic' },regex = True)
    df_bank = df_bank.replace({'basic.6y':'basic' },regex = True)
    df_bank = df_bank.replace({'basic.9y':'basic' },regex = True)
    education_categories = {0:'basic.',1:'high.school',2:'illiterate',3:'professional.course',4:'university.degree',5:'unknown'}
    dummies_education = pd.get_dummies(df_bank['education'].replace(education_categories))   
    df_bank = df_bank.join(dummies_education,rsuffix = '_education')
    df_bank = df_bank.drop(columns = ['education'])
      
    
    #dropping all unwanted columns
    df_bank = df_bank.drop(columns = ['pdays'])
    df_bank = df_bank.drop(columns = ['default'])
    df_bank = df_bank.drop(columns = ['emp.var.rate'])
    df_bank = df_bank.drop(columns = ['cons.price.idx'])
    df_bank = df_bank.drop(columns = ['cons.conf.idx'])
    df_bank = df_bank.drop(columns = ['euribor3m'])
    df_bank = df_bank.drop(columns = ['nr.employed'])
    df_bank = df_bank.drop(columns = [ col for col in df_bank.columns if 'unknown' in col])
    
    df_bank['y'] = np.where(df_bank['y' ] == 'yes',1,0 )   
    
    df_bank.to_csv("bank-additional/bank-additional/bank-additional-clean.csv",header=True, encoding = "utf-8")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    