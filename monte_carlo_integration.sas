 *q1;
data function;
do x = 0 to 20 by 0.1;
    y = x**3-216;
    output;
    end;
run;

proc sgplot data=function;
series x=x y=y;
run;


*q1b;



proc iml;
n = 100000;
use function;
read all into funct;


call randseed(123456);         /* set random number seed */
ua = j(n,1);                /* allocate */
call randgen(ua, "Uniform"); 
xa = 6+(10-6)*ua;     			/*set random numbers over the interval*/
ya = xa##3-216; /*function to integrate*/
ansa = (10-6)*(j(n,1,1))`*ya/n; /*MCintegration*/
print ansa;

xa = xa||funct;
colname = {"xa" "x" "y"};
*print xa[colname = colname];

create xplot from xa[colname=colname];
append from xa;
quit;
proc plot data=xplot formchar="|----|+|---+=|-/\<>*";
   plot x*y='*'
        xa='o' / overlay box;
run;
proc iml;
n=10000;
start integrate(a,b,n);
call randseed(123456);         /* set random number seed */
u = j(n,1);                /* allocate */
call randgen(u, "Uniform"); 
x = a+(b-a)*u;     			/*set random numbers over the interval*/
y = x##3-216; /*function to integrate*/
ans = (b-a)*(j(n,1,1))`*y/n; /*MCintegration*/
xn = x[1:20,];
print ans ;
finish;



call integrate(1,2,n);
call integrate(8,9,n);
call integrate(20,21,n);
call integrate(18,18,n);
call integrate(4,7,n);
call integrate(2,12,n);



quit;
