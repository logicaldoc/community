package com.logicaldoc.core.document.thumbnail;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.exec.Exec;

/**
 * Takes care of images thumbnail builder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class ImageThumbnailBuilder extends AbstractThumbnailBuilder {
	protected static Logger log = LoggerFactory.getLogger(ImageThumbnailBuilder.class);

	@Override
	public synchronized void buildThumbnail(String sid, Document document, String fileVersion, File src, File dest,
			int size, int quality) throws IOException {
		try {
			String outExt = FilenameUtils.getExtension(dest.getName().toLowerCase());
			ContextProperties conf = Context.get().getProperties();
			StringBuffer commandLine = new StringBuffer(conf.getProperty("converter.ImageConverter.path"));
			if ("png".equals(outExt))
				commandLine.append(" -alpha on ");
			commandLine.append(" -compress JPEG -quality " + quality);
			commandLine.append(" -resize x" + Integer.toString(size) + " " + src.getPath() + " " + dest.getPath());
			
			Exec.exec(commandLine.toString(), null, null, conf.getInt("converter.ImageConverter.timeout", 10));

			if (!dest.exists() || dest.length() == 0) {
				/*
				 * In case of multiple TIF pages, the output should be
				 * name-0.jpg, name-1.jpg ...
				 */
				final String basename = FilenameUtils.getBaseName(dest.getName());
				String testname = basename + "-0." + outExt;
				File test = new File(dest.getParentFile(), testname);
				if (test.exists()) {
					// In this case rename the first page with the wanted
					// destination file
					FileUtils.copyFile(test, dest);

					// And delete all other pages
					String[] pages = dest.getParentFile().list(new FilenameFilter() {
						@Override
						public boolean accept(File dir, String name) {
							return name.startsWith(basename + "-") && name.endsWith("." + outExt);
						}
					});
					for (String page : pages) {
						FileUtils.deleteQuietly(new File(page));
					}
				}
			}

			if (dest.length() < 1)
				throw new Exception("Empty thumbnail image");
		} catch (Throwable e) {
			throw new IOException("Error in IMG to JPEG conversion", e);
		}
	}
}