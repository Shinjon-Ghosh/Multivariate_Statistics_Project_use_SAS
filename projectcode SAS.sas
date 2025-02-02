PROC IMPORT DATAFILE="C:\Users\sghos10\Desktop\MS Thesis\MAT 456\Multivariate Statistics\Project\Employee Performance Metrics.xlsx"
    OUT=hr_dashboard_data
    DBMS=XLSX
    REPLACE;
RUN;



/* Step 1: Define formats for categorical variables */
PROC FORMAT;
    /* Format for Gender */
    VALUE $GenderFmt
        'Male' = '1'
        'Female' = '2';
        
    /* Format for Department */
    VALUE $DeptFmt
        'Marketing' = '1'
        'IT' = '2'
        'Sales' = '3'
        'HR' = '4'
        'Finance' = '5';
        
    /* Format for Position */
    VALUE $PosFmt
        'Analyst' = '1'
        'Manager' = '2'
        'Team Lead' = '3'
        'Junior Developer' = '4'
        'Senior Developer' = '5'
        'Intern' = '6';
RUN;



/* Step 2: Apply formats to the dataset */
DATA Employee_data;
    SET hr_dashboard_data;
    FORMAT 
        Gender $GenderFmt.
        Department $DeptFmt.
        Position $PosFmt.;
RUN;



/* Step 3: View the formatted dataset */
PROC PRINT DATA=Employee_data;
RUN;



/* Step 1: Check for outliers using summary statistics and box plots */
PROC UNIVARIATE DATA=Employee_data;
    VAR Projects_Completed Productivity Satisfaction_Rate Feedback_Score;
    ID Name; /* Identifies the observations in case of extreme values */
    ODS SELECT ExtremeObs; /* Show only the extreme observations */
RUN;


/* Step 1: Create a new dataset without outliers */
DATA Employee_noout;
    SET Employee_data;

    /* Exclude outliers based on conditions */
    IF NOT (
        (Projects_Completed > 23 OR Projects_Completed < 2) OR
        (Productivity > 95 OR Productivity < 1) OR
        (Satisfaction_Rate > 96 OR Satisfaction_Rate < 2) OR
        (Feedback_Score > 4.7 OR Feedback_Score < 1.2)
    );
RUN;

/* Boxplot */

PROC SGPLOT DATA=Employee_noout;
    VBOX Projects_Completed / CATEGORY=Gender;
RUN;

PROC SGPLOT DATA=Employee_noout;
    VBOX Productivity / CATEGORY=Gender;
RUN;

PROC SGPLOT DATA=Employee_noout;
    VBOX Satisfaction_Rate / CATEGORY=Gender;
RUN;

PROC SGPLOT DATA=Employee_noout;
    VBOX Feedback_Score / CATEGORY = Gender;
RUN;



PROC SGPLOT DATA=Employee_noout;
    VBOX Projects_Completed / CATEGORY=Position;
RUN;

PROC SGPLOT DATA=Employee_noout;
    VBOX Productivity / CATEGORY=Position;
RUN;

PROC SGPLOT DATA=Employee_noout;
    VBOX Satisfaction_Rate / CATEGORY=Position;
RUN;

PROC SGPLOT DATA=Employee_noout;
    VBOX Feedback_Score / CATEGORY = Position;
RUN;



PROC SGPLOT DATA=Employee_noout;
    VBOX Projects_Completed / CATEGORY=Department;
RUN;

PROC SGPLOT DATA=Employee_noout;
    VBOX Productivity / CATEGORY=Department;
RUN;

PROC SGPLOT DATA=Employee_noout;
    VBOX Satisfaction_Rate / CATEGORY=Department;
RUN;

PROC SGPLOT DATA=Employee_noout;
    VBOX Feedback_Score / CATEGORY = Department;
RUN;



/* Normality Check*/

PROC UNIVARIATE DATA=Employee_noout NORMAL;
    VAR Projects_Completed Productivity Satisfaction_Rate Feedback_Score;
    QQPLOT / NORMAL(MU=EST SIGMA=EST);
    HISTOGRAM / NORMAL(COLOR=RED W=2);
    INSET MEAN STD N / POSITION=NE; /* Place statistics like mean, std dev, and count */
RUN;


/* Draft */
proc IML;
reset log print;
x = Projects_Completed`||Productivity`||Satisfaction_Rate`||Feedback_Score`;
n= 156; p= 4; alpha = 0.05;
xb= J(1,n,1/n)*x;
xxb=J(n,1,1)*J(1,n,1/n)*x;
S=(x-xxb)`*(x-xxb)/(n-1);
D2=(((x-xxb)*inv(S))#(x-xxb))[,+];
rr=Rank(D2);
Qchi=Cinv((Rank(D2)-0.5)/n,p);
call PGRAF(M, '*', 'Qchi', 'D2', 'Chi-square plot');
quit;






/* Convert format Categorical Variables to Numeric */

DATA Employee_data_num;
    SET Employee_noout;
    
    /* Convert formatted categorical variables to numeric */
    Gender_num = INPUT(PUT(Gender, $GenderFmt.), BEST.);
    Department_num = INPUT(PUT(Department, $DeptFmt.), BEST.);
    Position_num = INPUT(PUT(Position, $PosFmt.), BEST.);
    
    /* Drop original character variables if not needed */
    DROP Gender Department Position;
RUN;


/* Step 1: Perform Box-Cox transformation */
PROC TRANSREG DATA=Employee_data_num;
    MODEL BOXCOX(Projects_Completed Productivity Satisfaction_Rate Feedback_Score) = IDENTITY(Gender_num Department_num Position_num);
    OUTPUT OUT=Employee_BCT;
RUN;

/* Step 2: Inspect the transformed dataset */
PROC PRINT DATA=Employee_BCT;
RUN;


/* Normality Check again */
PROC UNIVARIATE DATA=Employee_BCT NORMAL;
    VAR TProjects_Completed TProductivity TSatisfaction_Rate TFeedback_Score;
    QQPLOT / NORMAL(MU=EST SIGMA=EST);
    HISTOGRAM / NORMAL(COLOR=RED W=2);
    INSET MEAN STD N / POSITION=NE; /* Place statistics like mean, std dev, and count */
RUN;


/* MANOVA */
PROC GLM DATA=Employee_BCT;
    CLASS Gender_num Position_num Department_num; 
    MODEL TProductivity TSatisfaction_Rate = Gender_num Position_num Department_num;
    MANOVA H=Gender_num Position_num Department_num;
    MEANS Gender_num Position_num Department_num / Tukey;
RUN;
QUIT;



PROC GLM DATA=Employee_noout;
    CLASS Gender; /* Independent variable */
    MODEL Projects_Completed Productivity Satisfaction_Rate Feedback_Score = Gender; /* Dependent variables */
    MANOVA / PRINTE PRINTH;
    MEANS Gender / HOVTEST=BF; /* Homogeneity of variance test (Brown-Forsythe) */
RUN;


PROC GLM DATA=Employee_noout;
    CLASS Gender; /* Independent variable */
    MODEL log_Projects log_Productivity log_Satisfaction log_Feedback = Gender; /* Dependent variables */
    MANOVA / PRINTE PRINTH;
    MEANS Gender / HOVTEST=BF; /* Homogeneity of variance test (Brown-Forsythe) */
RUN;


PROC GLM DATA=Employee_noout;
    CLASS Gender; /* Independent variable */
    MODEL sqroot_projects sqroot_productivity sqroot_satisfaction sqroot_feedback = Gender; /* Dependent variables */
    MANOVA / PRINTE PRINTH;
    MEANS Gender / HOVTEST=BF; /* Homogeneity of variance test (Brown-Forsythe) */
RUN;


PROC GLM DATA=Employee_noout;
    CLASS Position; /* Independent variable */
    MODEL Projects_Completed Productivity Satisfaction_Rate Feedback_Score = Position; /* Dependent variables */
    MANOVA / PRINTE PRINTH;
    MEANS Position / HOVTEST=BF; /* Homogeneity of variance test (Brown-Forsythe) */
RUN;

PROC GLM DATA=Employee_noout;
    CLASS Position; /* Independent variable */
    MODEL log_Projects log_Productivity log_Satisfaction log_Feedback = Position; /* Dependent variables */
    MANOVA / PRINTE PRINTH;
    MEANS Position / HOVTEST=BF; /* Homogeneity of variance test (Brown-Forsythe) */
RUN;

PROC GLM DATA=Employee_noout;
    CLASS Position; /* Independent variable */
    MODEL sqroot_projects sqroot_productivity sqroot_satisfaction sqroot_feedback = Position; /* Dependent variables */
    MANOVA / PRINTE PRINTH;
    MEANS Position / HOVTEST=BF; /* Homogeneity of variance test (Brown-Forsythe) */
RUN;


PROC GLM DATA=Employee_noout;
    CLASS Department; /* Independent variable */
    MODEL Projects_Completed Productivity Satisfaction_Rate Feedback_Score = Department; /* Dependent variables */
    MANOVA / PRINTE PRINTH;
    MEANS Department / HOVTEST=BF; /* Homogeneity of variance test (Brown-Forsythe) */
RUN;


PROC GLM DATA=Employee_noout;
    CLASS Gender; /* Independent variable: Gender */
    MODEL Projects_Completed Productivity Satisfaction_Rate Feedback_Score = Gender;
    MANOVA H=Gender / PRINTE PRINTH;
    
RUN;
QUIT;

PROC GLM DATA=Employee_noout;
    CLASS Position; /* Independent variable: Gender */
    MODEL Projects_Completed Productivity Satisfaction_Rate Feedback_Score = Position;
    MANOVA H=Position / PRINTE PRINTH;
    
RUN;
QUIT;

PROC GLM DATA=Employee_noout;
    CLASS Department; /* Independent variable: Gender */
    MODEL Projects_Completed Productivity Satisfaction_Rate Feedback_Score = Department;
    MANOVA H=Department / PRINTE PRINTH;
    
RUN;
QUIT;


