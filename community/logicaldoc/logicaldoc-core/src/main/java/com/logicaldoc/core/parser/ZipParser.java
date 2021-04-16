package com.logicaldoc.core.parser;

import java.io.File;
import java.io.InputStream;
import java.util.List;
import java.util.Locale;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ZipUtil;

/**
 * Class for parsing text (*.txt) files.
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.5
 */
public class ZipParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(ZipParser.class);

	@Override
	public void internalParse(InputStream input, String filename, String encoding, Locale locale, String tenant,
			Document document, String fileVersion, StringBuffer content) throws Exception {
		File zipFile = File.createTempFile("parsezip", "zip");
		try {
			FileUtil.writeFile(input, zipFile.getAbsolutePath());
			ZipUtil zipUtil = new ZipUtil();
			List<String> entries = zipUtil.listEntries(zipFile);
			for (String line : entries) {
				content.append(line);
				content.append("\n");
			}
		} finally {
			if (zipFile != null)
				FileUtil.strongDelete(zipFile);
		}
	}
}