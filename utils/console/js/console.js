(function(){

    function sendRequest(method, resource, data) {

        var client = $('#console-client').val() || 'demo',
            host   = 'http://' + ($('#console-host').val() || 'localhost:3010');

        Trombone.request({
            host     : host,
            key      : $('#console-key').val(),
            client   : client,
            type     : method,
            resource : resource,
            data     : data,
            nonce    : Date.now()/10 | 0,
            success  : function(resp) {
                msgout(resp);
            },
            error    : function(e) {
                try {
                    msgout(JSON.parse(e.responseText));
                 } catch (err) {
                    var msg = '';
                    if (e.responseText) {
                        msg = e.responseText;
                    } else if (!e.status) {
                        msg = 'No service.';
                    } else {
                        msg = 'Error: ' + e.status;
                    }
                    $("#console-out").html(msg);
                }
            }
        });

    }

    function msgout(msg) {
        var tbl = json2tbl.build(msg, {
            cssClass: 'table'
        });
        $('#console-out').html(tbl);
    }

    $(document).ready(function() {

        var method = 'GET';

        $('.method-selector a').click(function() {
            method = $(this).html();
            $('#console-action').html(method);
            switch (method) {
                case 'POST':
                case 'PUT':
                case 'DELETE':
                case 'PATCH':
                    $('#console-wrapper, #console-btn-wrapper').show();
                    break;
                default:
                    $('#console-wrapper, #console-btn-wrapper').hide();
            }
        });

        $('#console-ping').click(function() {
            sendRequest('GET', '/ping');
        });

        $('#console-send').click(function() {

            var resource = $('#console-resource').val().replace(/^\/|\/$/g, ''),
                data;

            switch (method) {
                case 'POST':
                case 'PUT':
                case 'DELETE':
                case 'PATCH':
                    data = $('#console-payload').val();
                    break;
                default:
                    data = '';
            }

            sendRequest(method, resource, data);

        });

        $('#console-json-validate').click(function() {
            var data = $('#console-payload').val();
            if (!data) {
                $('#console-out').html('No input.');
                return;
            }
            try {
                JSON.parse(data);
                $('#console-out').html('OK.');
            } catch (err) {
                $('#console-out').html('Invalid JSON.');
            }
        });

        $('#console-buffer-reset').click(function() {
            $('#console-payload').val('');
        });

        $('#console-output-clear').click(function() {
            $('#console-out').empty();
        });

    });

}());
