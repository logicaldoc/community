package com.logicaldoc.util.http;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

import org.apache.commons.io.IOUtils;
import org.apache.hc.core5.http.ClassicHttpResponse;
import org.apache.hc.core5.http.HttpEntity;

/**
 * A HTTP response handler that writes the response into a given output stream
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.3
 */
public class StreamHttpClientResponseHandler extends BaseHttpClientResponseHandler<String> {

    private OutputStream stream;

    public StreamHttpClientResponseHandler(OutputStream stream) {
        super();
        this.stream = stream;
    }

    @Override
    public String handleResponse(ClassicHttpResponse response) throws IOException {
        if (response.getEntity() != null) {
            return handleEntity(response.getEntity());
        } else {
            IOUtils.write(response.getReasonPhrase(), stream, StandardCharsets.UTF_8);
            return response.getReasonPhrase();
        }
    }

    @Override
    public String handleEntity(HttpEntity entity) throws IOException {
        IOUtils.copy(entity.getContent(), stream);
        return "";
    }
}