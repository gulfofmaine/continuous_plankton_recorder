function [TS,varargout]=CalProc(CPRdat,splinebins,seasonbins)
%CALPROC--process Calanus data by removing seasonal cycle
%
%[TS,SD, N]=CalProc(CPRdat,splinebins,seasonbins)
%
%Processes the CPR data in CPRdat.  CPRdat should be an m-by-5 matrix of CPR
%observations: [year, jday, lat, lon, #].  First, a cubic spline is fit to the
%seasonal cycle.  The seasonal cycle is removed from the data and mean anomalies
%are computed for each year.  Two vectors define the resolution of the spline
%and the division of each year. The resolution of the spline is determined by
%a 1-by-p vector splinebins (in days).  The spline is produced by computing
%the mean value of the data between splinebins(j) and splinebins(j+1). 
%If splinebins is an integer p, then the year is divided into p equal periods.
%This value is assumed to occur on a day within this interval (the mean of the
%sample dates).  This procedure creates a set of p-1 points.   The spline
%is fit to these points using csape. The spline is subtracted from each
%observation to create a series of anomalies.  If seasonbins is not provided,
%Then the anomalies are returned in a matrix 
%			[CPRdat(:,1)+CPRdat(:,2)/365, anomalies]
%If season bins is an integer q, then the year is divided into q equal periods
%and the mean of these periods within each year is produced.  The result will
%be a y-by-(q+1) matrix
%			[year, mean in period 1, mean in period 2, ..., mean in period q]
%Seasonbins can also be a 1-by-(q+1) vector defining the boundaries of the
%bins.
%
% 10/3/02--splinebins can be matlab polynomial (like output of csape).  If this
%          is the case, then that polynomial is used, rather than creating one.
%
%The optional outputs SD will contain the standard deviation of the estimates in
%TS and N will contain the number of samples.
%

[m,n]=size(CPRdat);
if(n~=5)
	error('dat format requires 5 columns: [year, jday, lat, lon, #]');
end

if(nargin<3)
	parseanomalies=0;%just  return the anomalies
else
	parseanomalies=1;%parse into seasonal periods
   q=length(seasonbins);
   if(q==1)
   	seasonbins=linspace(0,365,seasonbins+1);
		q=length(seasonbins);
   end
end

if(isstruct(splinebins))
	%disp('splinebins is a struct, assuming its a spline');
	sp=splinebins;
else

	p=length(splinebins);
	if(p==1)
		splinebins=linspace(0,365,splinebins+1);
		p=length(splinebins);
	end
	% Create seasonal cycle

	mmn=zeros(p-1,2);
	somebad=0;
	for j=1:p-1
	   I=find(CPRdat(:,2)>=splinebins(j) & CPRdat(:,2)<splinebins(j+1));
	   if(~isempty(I))
           J=find(~isnan(CPRdat(I,2)));
           mmn(j,1)=mean(CPRdat(I(J),2));
           J=find(~isnan(CPRdat(I,5)));
           mmn(j,2)=mean(CPRdat(I(J),5));
	      %mmn(j,:)=nanmean(CPRdat(I,[2,5]));
	   else
	      mmn(j,:)=[nan nan];
	      somebad=1;
	   end
	end
	if(somebad)
	   I=find(~isnan(mmn(:,1)));
	   if(~isempty(I))
	      mmn=mmn(I,:);
	   else
	      error('Unable to create spline--no data in bins!');
	   end
	end

	%copy mmn to create 3 cycles--ensures that the spline is periodic
	%between days 0 and 365.
	mmn2=[[mmn(:,1)-365;mmn(:,1);mmn(:,1)+365],[mmn(:,2);mmn(:,2);mmn(:,2)]];

	sp=csape(mmn2(:,1),mmn2(:,2));%creates spline
end



TS=(0:365)';
TS(:,2)=ppval(sp,TS);%fnval(TS,sp);%replace fnval with ppval
%2) Create anomalies
CPRdat(:,5)=CPRdat(:,5)-ppval(sp,CPRdat(:,2));%fnval(CPRdat(:,2),sp);

if(~parseanomalies)
	TS=zeros(m,2);
   TS(:,1)=CPRdat(:,1)+CPRdat(:,2)/365;
   TS(:,2)=CPRdat(:,5);
   varargout{1}=sp;
else
   yr=min(CPRdat(:,1)):max(CPRdat(:,1));%years
   nyr=length(yr);
   TS=ones(nyr,q)*nan;
   TS(:,1)=yr(:);
   if(nargout > 1)
       SE=TS;
   end
   if(nargout > 2)
       N=TS;
   end
   CPRdat=sortrows(CPRdat,[1 2]);%sort the data for faster searching
   st=1;
   en=length(CPRdat(:,1));
   for j=1:nyr;
   	I=find(CPRdat(st:en,1)==yr(j));
      if(~isempty(I))
      	I=I+st-1;
      	for k=1:q-1
         	J=find(CPRdat(I,2)>=seasonbins(k) & CPRdat(I,2)<seasonbins(k+1));
            if(~isempty(J))
                K=find(~isnan(CPRdat(I(J),5)));
                TS(j,1+k)=mean(CPRdat(I(J(K)),5));
            	%TS(j,1+k)=nanmean(CPRdat(I(J),5));
                if(nargout > 1)
                    SE(j,1+k)=std(CPRdat(I(J(K)),5));
                    %SE(j,1+k)=nanstd(CPRdat(I(J),5));
                end
                if(nargout > 2)
                    N(j,1+k)=length(J);
                end
            end
         end
         st=I(end)+1;
      end
   end
   varargout{1}=SE;
   varargout{2}=N;
   varargout{3}=sp;
end
