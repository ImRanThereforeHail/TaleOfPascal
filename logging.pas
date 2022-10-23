unit Logging;

interface
var log_file : TextFile;
implementation

initialization
Assign(log_file, 'log.txt');
Rewrite(log_file);
finalization
Close(log_file);
end.