-define(ERROR(Message), ?ERROR(Message, [])).
-define(ERROR(Message, Args), error_logger:error_msg(Message, Args)).