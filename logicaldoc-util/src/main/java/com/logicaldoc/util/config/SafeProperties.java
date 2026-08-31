package com.logicaldoc.util.config;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.nio.charset.Charset;

import com.logicaldoc.util.spring.Context;

/**
 * Retrieves low-level read-only properties that cannot be retrieved with
 * standard {@link ContextProperties}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3.1
 */
public class SafeProperties extends ContextProperties {

    private static final long serialVersionUID = 1L;

    private static SafeProperties instance;

    public static SafeProperties get() throws IOException {
        if (instance == null)
            instance = new SafeProperties();
        return instance;
    }

    private SafeProperties() throws IOException {
        super(new File(
                "%s/%s".formatted(Context.get().getConfig().getProperty("LDOCHOME"), "/conf/context-safe.properties")));
    }

    @Override
    protected File detectOverrideFile() {
        return null;
    }

    @Override
    public synchronized void write() throws IOException {
        throw new IOException("write not supported");
    }

    @Override
    public void save(OutputStream out, String comments) {
        // Do nothing
    }

    @Override
    public void store(Writer writer, String comments) throws IOException {
        write();
    }

    @Override
    public void storeToXML(OutputStream os, String comment) throws IOException {
        write();
    }

    @Override
    public void storeToXML(OutputStream os, String comment, String encoding) throws IOException {
        write();
    }

    @Override
    public void storeToXML(OutputStream os, String comment, Charset charset) throws IOException {
        write();
    }
}