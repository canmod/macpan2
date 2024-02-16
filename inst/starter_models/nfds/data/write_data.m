% Short script to write data locally from https://github.com/carolinecolijn/optimvaccine
% Save .mat elements as individual csv's so they can easily be read in R
%
% Author: Jennifer Freeman

%------------- BEGIN CODE --------------

% note you need to use the url for the 'raw' data
load(websave('massdata',"https://github.com/carolinecolijn/optimvaccine/raw/master/massdata.mat"));
fn = fieldnames(massdata);
for i=1:numel(fn)
    if( isnumeric(massdata.(fn{i})) || ischar(massdata.(fn{i})) )
        writematrix(massdata.(fn{i}),strcat(fn{i},'.csv'));
    elseif(isstruct(massdata.(fn{i})))
            s_fn = fieldnames(massdata.(fn{i}));
            for j = 1:numel(s_fn)
                writematrix(massdata.(fn{i}).(s_fn{j}),strcat(fn{i},'.',s_fn{j},'.csv'));   
            end
    else
        writecell(massdata.(fn{i}),strcat(fn{i},'.csv'));
    end
end

% should also do for Maela data set: https://github.com/carolinecolijn/optimvaccine/raw/master/redmaela.mat

%------------- END OF CODE --------------