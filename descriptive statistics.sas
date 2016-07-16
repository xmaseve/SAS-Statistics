data Blood_Pressure;
   call streaminit(37373);
   do Drug = 'Placebo','Drug A','Drug B';
      do i = 1 to 20;
         Subj + 1;
         if mod(Subj,2) then Gender = 'M';
         else Gender = 'F';
         SBP = rand('normal',130,10) +
               7*(Drug eq 'Placebo') - 6*(Drug eq 'Drug B');
         SBP = round(SBP,2);
         DBP = rand('normal',80,5) +
               3*(Drug eq 'Placebo') - 2*(Drug eq 'Drug B');
         DBP = round(DBP,2);
         if Subj in (5,15,25,55) then call missing(SBP, DBP);
         if Subj in (4,18) then call missing(Gender);
         output;
      end;
   end;
   drop i;
run;

/*Continuous Variables*/
proc means data=Blood_Pressure n nmiss mean std printalltypes maxdec=3;
	class Drug;
	var SBP DBP;
run;

ods trace on;
proc univariate data=Blood_Pressure;
	var SBP DBP;
	histogram SBP / midpoints=100 to 170 by 5 normal;
	probplot / normal (mu=est sigma=est);
run;

ods trace off;

/*Use the box plot to detect outliers*/
proc sgplot data=Blood_Pressure;
	vbox SBP / category=Drug datalabel=Subj; /*datalable= identify specific outliers*/
run;

proc surveyselect data=Blood_Pressure out=one
	method=urs outhits sampsize=30 seed=1234567;
run;

proc print data=one;run;

/*Categorical Variables*/
proc freq data=Blood_Pressure;
	table gender drug/ nocum missing;
run;

proc format;
value $gender 'M' = 'Male'
	          'F' = 'Female';
value sbpgroup low - 140 = 'Normal'
			   141 - high = 'High';
value dbpgroup low - 80 = 'Normal'
               81 - high = 'Hign';
run;

proc freq data=Blood_Pressure;
	table gender SBP DBP/ nocum;
	format gender $gender. SBP sbpgroup. DBP dbpgroup.;
run;

proc freq data=Blood_Pressure;
	tables gender * drug;
run;
	

proc format;
   value yesno 1 = 'Yes'
               0 = 'No';
data Store;
   length Region $ 5;
   call streaminit(57676);
   do Transaction = 1 to 200;
      R = ceil(rand('uniform')*10);
      select(R);
         when(1) Region = 'East';
         when(2) Region = 'West';
         when(3) Region = 'North';
         when(4) Region = 'South';
         otherwise;
      end;
      Advertising = rand('bernouli',.6);
      if rand('uniform') lt .6 then Gender = 'Female';
         else Gender = 'Male';
      Book_Sales = abs(round(rand('normal',250,50) + 30*(Gender = 'Female')
                    + 30*Advertising,10)) ;
      Music_Sales = abs(round(rand('uniform')*40 + rand('normal',50,5)
         + 30*(Region = 'East' and Gender = 'Male')
         - 20*(Region = 'West' and Gender = 'Female'),5) + 10*Advertising);
      Electronics_Sales = abs(round(rand('normal',300,60) + 70*(Gender = 'Male')
       + 55*Advertising + 50*(Region = 'East') - 20*(Region = 'South') 
       + 75*(Region = 'West'),10));
      Total_Sales = sum(Book_Sales,Music_Sales,Electronics_Sales);
   output;
   end;
   drop R;
   format Book_Sales Music_Sales Electronics_Sales Total_Sales dollar9.
          Advertising yesno.;
run;

data exercise;
   call streaminit(7657657);
   do Subj = 1 to 50;
      Age = round(rand('normal',50,15));
      Pushups = abs(int(rand('normal',40,10) - .30*age));
      Rest_Pulse = round(rand('normal',50,8) + .35*age);
      Max_Pulse = round(rest_pulse + rand('normal',50,5) - .05*age);
      Run_Pulse = round(max_pulse - rand('normal',3,3));
      output;
   end;
run;

ods graphics on;
proc ttest data=exercise h0=50 sides=2 alpha=0.5;
	var age;
run;
ods graphics off;

proc glm data=store;
	class Region;
	model Electronics_Sales = Region / ss3;
	means Region / hovtest;
run;

