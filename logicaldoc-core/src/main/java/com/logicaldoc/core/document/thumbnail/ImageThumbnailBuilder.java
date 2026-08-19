package com.logicaldoc.core.document.thumbnail;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.conversion.ImageConverter;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.io.FileUtil;

/**
 * Takes care of images thumbnail builder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class ImageThumbnailBuilder extends AbstractThumbnailBuilder {
    private static final Logger log = LoggerFactory.getLogger(ImageThumbnailBuilder.class);

    @Override
    public synchronized void buildThumbnail(
            String sid,
            Document document,
            String fileVersion,
            File src,
            File dest,
            int size,
            int quality) throws IOException {

        String outExt = FileUtil.getExtension(dest.getName().toLowerCase());
        List<String> arguments = new ArrayList<>();
        if ("png".equals(outExt)) {
            arguments.add("-alpha");
            arguments.add("on");
        }
        arguments.addAll(
                List.of("-compress", "JPEG", "-quality", Integer.toString(quality), "-resize", "x%d".formatted(size)));

        log.debug("Executing: {}", arguments);
      
        new ImageConverter().convert(src, dest, arguments, null);

        if (!dest.exists() || dest.length() == 0) {
            /*
             * In case of multiple TIF pages, the output should be name-0.jpg,
             * name-1.jpg ...
             */
            final String basename = FileUtil.getBaseName(dest.getName());
            String testname = "%s-0.%s".formatted(basename, outExt);
            File test = new File(dest.getParentFile(), testname);
            if (test.exists()) {
                // In this case rename the first page with the wanted
                // destination file
                FileUtils.copyFile(test, dest);

                // And delete all other pages
                String[] pages = dest.getParentFile().list((dir, name) -> name.startsWith("%s-".formatted(basename))
                        && name.endsWith(".%s".formatted(outExt)));
                for (String page : pages) {
                    FileUtils.deleteQuietly(new File(page));
                }
            }
        }

        if (dest.length() < 1)
            throw new IOException("Empty thumbnail image");
    }

    public void convert(File src, File dest, Integer size, Integer quality) {

    }
}