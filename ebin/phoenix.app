{application,phoenix,
             [{description,"Phoenix server"},
              {vsn,"0.0.0"},
              {modules,[itc,itc_server,phoenix,phoenix_app,phoenix_sup,
                        phoenix_user,phoenix_user_controller,ws_handler]},
              {registered,[phoenix,phoenix_sup]},
              {applications,[kernel,stdlib,cowlib,ranch,cowboy]},
              {mod,{phoenix_app,[]}},
              {env,[]}]}.
