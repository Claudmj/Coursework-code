quit ;
dm 'odsresults;clear';
title ;
title1;
libname ekt725 "C:\Users\claud\Documents\University\Honors\Semester 2\EKT 725\Assignments\Assignment 4";

PROC IMPORT OUT= EKT725.knn1
DATAFILE= "C:\Users\claud\Documents\University\Honors\Semester 2\EKT 725\Assignments\Assignment 4\q3.xlsx"
DBMS=EXCEL REPLACE;
GETNAMES=YES;
MIXED=NO;
SCANTEXT=YES;
USEDATE=YES;
SCANTIME=YES;
RUN;


proc iml  ;

use ekt725.knn1 ;
read all into x ;

x=x[1:500,] ;
*print x;
n=nrow(x) ;

*print x ;
call sort(x,{1}) ;


start knnd(x,k) ;
 d = distance(x) ;
* print d ;
	 do i = 1 to nrow(x) ;
	 d1=d[,i] ;
*	 print d1 ;
	 call sort(d1,{1}) ;
*     print d1 ;
	 fxd = 1 / (d1[k])##2 ;
	 fx = fx // fxd ;
*	 print fx ;
	 end ;
return fx ;
finish knnd ;





start clus(x,fx,k,n) ;
d=distance(x);
do i = 1 to n;
 stp=0 ;
 nn=i ;
 kk=0 ;
 do j = 1 to n while (stp=0) ;
*  print i nn ;
  kk=kk+1;
  d1=d[,nn] || fx ;
  call sort(d1,{1}) ;
  d1 = d1[1:k,];
*  print d1 ;
  mfx = (d1[,5])[<:>] ;
*  print i d1 mfx ;
  if kk=n then stp=1 ;
  if mfx=i then stp=1 ;
  nn=d1[mfx,2] ;
*  print nn stp;
 end ;
 clus = clus // (i || x[i,] || nn  ) ;
end ;
nm={"nr" "x" "fx" "clus"} ;
fclus = fx || clus[,4] ;
return fclus ;
finish clus ;
kn = 10;
pn = 0.8*n;
id = 1:n;
idk = 1:pn;
*kv = { 10, 50, 100, 150, 200, 250, 300, 400, 500, 600};
kv = (sample(idk,kn,"WOR"))`;
*print kv;
I = j(n,n,0);
Sij = j(n,n,0);
newI = j(n,n,0);
newS = j(n,n,0);
*idx = j(pn,kn,0);
do ki = 1 to kn; 
k = kv[ki,];
idx = sample(id,pn,"WOR")`;
*call sort(idx);
x1 = x;
s = x1[idx,];
*print x;
newI[idx,idx]=I[idx,idx]+1;
I = newI;
fx = knnd(x,k) ;
fx = id`|| x || fx ;
fclus = clus(x,fx,k,n);
*print fclus  ;
cluster = (id`||x||fclus[,5]);
clusteri = (idx||s||fclus[idx,5]);
call sort(clusteri,{1}) ;
print clusteri;
*print clusteri;
do ii =1 to pn;
do jj =1 to pn;
*if cluster[idx,4] = cluster[idx,4] then newS[idx,idx]=Sij[idx,idx]+1;
if clusteri[ii,4] = clusteri[jj,4] then newS[clusteri[ii,1],clusteri[jj,1]]=Sij[clusteri[ii,1],clusteri[jj,1]]+1;
end;
end;
Sij = newS;
end;
C = newS/newI;
print Sij I C;
C = (1:n)`||C;
i_x = (1:n)`||x;
print i_x;
nm = {"COL1" "x1" "x2"};


create ekt725.dend from C ;
append from C ;


create ekt725.one from i_x[colname=nm] ;
append from i_x ;

quit ;

proc cluster
data = ekt725.dend
outtree = ekt725.treedata
method = average;
id COL1;
run;

proc tree
data = ekt725.treedata
out = ekt725.cluster_data
nclusters = 3;
id COL1;
quit;


proc sort data=ekt725.Cluster_data ;
 by COL1 ;
run ;


  DATA ekt725.three;
   MERGE ekt725.one ekt725.Cluster_data;
   BY COL1;
  *PROC PRINT DATA=three; 
  RUN;



 /* Add a title to the graph */
title1 'Clusters'
       ;

 /* Define the symbol shape */
symbol1 interpol=none width=3
 		color=purple
        value=dot
        height=2;
symbol2 interpol=none width=3
 		color=red
        value=dot
        height=2;

symbol3 interpol=none width=3
 		color=green
        value=dot
        height=2;




 /* Create the graph */
proc gplot data= ekt725.Three;
   plot x1*x2=CLUSTER; 
run;
quit;

*run ;
