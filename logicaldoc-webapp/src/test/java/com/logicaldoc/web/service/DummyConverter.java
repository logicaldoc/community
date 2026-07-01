package com.logicaldoc.web.service;

import java.io.File;
import java.io.IOException;
import java.util.List;

import com.logicaldoc.core.conversion.AbstractFormatConverter;
import com.logicaldoc.core.document.Document;

/**
 * A do-nothing conveter just for testing purposes
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3
 *
 */
public class DummyConverter extends AbstractFormatConverter {

    
    
    @Override
    public List<String> getParameterNames() {
        return List.of("timeout");
    }

    @Override
    protected void internalConvert(String sid, Document document, File src, File dest) throws IOException {
        // Do nothing
    }
}