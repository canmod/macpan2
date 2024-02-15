% Simple script to simulate data from the Massachusets population
% after introduction of the PCV7 vaccine.
% 
% This script is modified from: 
%   https://github.com/carolinecolijn/optimvaccine/blob/master/example_script_bo.m
%   https://github.com/carolinecolijn/optimvaccine/blob/master/fitness_for_bo.m
%   https://github.com/carolinecolijn/optimvaccine/blob/master/runODEmodel.m
%
% Author: Jennifer Freeman

%------------- BEGIN CODE --------------

%% get population parameter inputs

% note you need to use the url for the 'raw' data
load(websave('massdata',"https://github.com/carolinecolijn/optimvaccine/raw/master/massdata.mat"));

% total number of serotypes in the population (last one is NT)
nAllSeros=length(massdata.seronames)-1; 

% parameter inputs
LL= size(massdata.G,1);   % number of isolates
sigma=massdata.sigma;     % related to strength of NFDS (see rho)
tmax=12*10;               % NOTE HARD CODED that we solve the model for ONLY 10 YEARS. 
vacfactor=massdata.v;     % vaccine efficacy
m=massdata.m;             % genetic migration
K=100000;                 % carrying capacity
x0=massdata.ics;          % intial conditions
rho=log(1+sigma);

% set ODE solver options
ops=odeset('AbsTol',1e-8,'RelTol',1e-5,'NonNegative', 1:LL,'BDF','off');

%% vaccine design

% build vaccine vector of all serotypes
serovt=zeros(1,nAllSeros+1);
% serotypes in PCV7 vaccine
PCV7={'4', '6B', '9V', '14', '18C', '19F', '23F'};

% set up the serotypes
includedSTs=PCV7; 
excludedSTs=[];
varyingSTs=[]; 

% set 1 for all included STs
for n=1:length(includedSTs)
     serovt(strcmp(massdata.seronames,includedSTs{n}))=1;
end

% serotype to strain conversion
VT=massdata.SerotypeToStrain*serovt';  % serovt: is serotype j in the vaccine?
VT(VT>0)=1;
combiVT=vacfactor*VT;

% model vaccine term
rr=-log(1-combiVT); 

%% solve ODE

% this ODE comes from the standard discretization (in reverse)
% version for variable strength of nfds
modifiedGT=repmat(massdata.locusweights, 1, size(massdata.G,1)).*massdata.G';
diffxt=@(t,x) x ...
        .*(log(K/sum(x)) - rr + rho* (massdata.weighteddev-massdata.G*modifiedGT*(x/sum(x)))) + m*ones(size(x)) ;

% ODE solution
tspan = [1:tmax];
[t,xsol]=ode15s(diffxt, tspan, x0, ops);  % def not at eq after 10000 mathematically but invasiveness equilibrates soon

writematrix(xsol,'simdata_PCV7.csv');

%------------- END OF CODE --------------