package com.logicaldoc.core.document.thumbnail;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.exec.Exec;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;

/**
 * Takes care of images thumbnail builder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class ImageThumbnailBuilder extends AbstractThumbnailBuilder {
	private static final Logger log = LoggerFactory.getLogger(ImageThumbnailBuilder.class);

	@Override
	public synchronized void buildThumbnail(String sid, Document document, String fileVersion, File src, File dest,
			int size, int quality) throws IOException {

		String outExt = FileUtil.getExtension(dest.getName().toLowerCase());
		ContextProperties conf = Context.get().getProperties();
		List<String> commandLine = new ArrayList<>();
		commandLine.add(conf.getProperty("converter.ImageConverter.path"));
		if ("png".equals(outExt)) {
			commandLine.add("-alpha");
			commandLine.add("on");
		}
		commandLine.addAll(List.of("-compress", "JPEG", "-quality", Integer.toString(quality), "-resize",
				"x" + Integer.toString(size), src.getPath(), dest.getPath()));

		log.debug("Executing: {}", commandLine);

		new Exec().exec(commandLine, null, null, conf.getInt("converter.ImageConverter.timeout", 10));

		if (!dest.exists() || dest.length() == 0) {
			/*
			 * In case of multiple TIF pages, the output should be name-0.jpg,
			 * name-1.jpg ...
			 */
			final String basename = FileUtil.getBaseName(dest.getName());
			String testname = basename + "-0." + outExt;
			File test = new File(dest.getParentFile(), testname);
			if (test.exists()) {
				// In this case rename the first page with the wanted
				// destination file
				FileUtils.copyFile(test, dest);

				// And delete all other pages
				String[] pages = dest.getParentFile()
						.list((dir, name) -> name.startsWith(basename + "-") && name.endsWith("." + outExt));
				for (String page : pages) {
					FileUtils.deleteQuietly(new File(page));
				}
			}
		}

		if (dest.length() < 1)
			throw new IOException("Empty thumbnail image");
	}
}