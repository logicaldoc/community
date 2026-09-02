package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.apache.commons.collections4.CollectionUtils;

import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.util.exec.Exec;
import com.logicaldoc.util.io.FileUtil;

/**
 * Converter to convert image files in PDF
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public class ImageConverter extends AbstractFormatConverter {

    @Override
    public void internalConvert(String sid, AbstractDocument document, File src, File dest) throws IOException {
        String ext = FileUtil.getExtension(dest.getName()).toLowerCase();
        if (!"pdf".equals(ext))
            throw new IOException("Unable to convert image to %s".formatted(ext));

        try {
            new Exec().exec(List.of(getParameter("path"), "-compress", "JPEG", src.getPath(), dest.getPath()), null,
                    null, getTimeout());

            if (!dest.exists() || dest.length() < 1)
                throw new IOException("Empty conversion");
        } catch (IOException ioe) {
            throw ioe;
        } catch (Exception e) {
            throw new IOException("Error in IMG to PDF conversion", e);
        }
    }

    private int getTimeout() {
        int timeout = 10;
        try {
            timeout = Integer.parseInt(getParameter("timeout"));
        } catch (Exception t) {
            // Nothing to do
        }
        return timeout;
    }

    @Override
    public List<String> getParameterNames() {
        return Arrays.asList("path", "timeout");
    }

    public void convert(File src, File dest, List<String> arguments, Integer timeout) throws IOException {
        loadParameters();

        List<String> commandLine = new ArrayList<>();
        commandLine.add(getParameter("path"));
        if (CollectionUtils.isNotEmpty(arguments))
            commandLine.addAll(arguments);
        if (src != null)
            commandLine.add(src.getAbsolutePath());
        commandLine.add(dest.getAbsolutePath());

        new Exec().exec(commandLine, null, null, Optional.ofNullable(timeout).orElse(getTimeout()));
    }
}