package com.logicaldoc.core.parser;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.Context;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.charset.CharsetDetector;
import com.logicaldoc.util.charset.CharsetMatch;
import com.logicaldoc.util.io.IOUtil;

/**
 * Class for parsing text (*.txt) files.
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - Logical Objects
 * @since 3.5
 */
public class TXTParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(TXTParser.class);

	@Override
	public void parse(File file) {
		FileInputStream fis = null;
		BufferedInputStream bis = null;
		try {
			fis = new FileInputStream(file);
			bis = new BufferedInputStream(fis);

			if (StringUtils.isEmpty(getEncoding())) {
				// Determine the most probable encoding
				try {
					CharsetDetector cd = new CharsetDetector();
					cd.setText(bis);
					CharsetMatch cm = cd.detect();
					if (cm != null) {
						if (Charset.isSupported(cm.getName()))
							setEncoding(cm.getName());
					}
				} catch (Throwable th) {
					log.warn("Error during TXT fileNameCharset detection", th);
				}
			}
			parse(bis);
		} catch (Throwable ex) {
			log.warn("Failed to extract TXT text content", ex);
		} finally {
			try {
				if (bis != null)
					bis.close();
				if (fis != null)
					fis.close();
			} catch (IOException e) {
				log.warn(e.getMessage(), e);
			}
		}
	}

	@Override
	public void internalParse(InputStream input) {
		try {
			if (input != null)
				content.append(StringUtil.writeToString(getLimitedStream(input), getEncoding()));
		} catch (UnsupportedEncodingException e) {
			log.warn("Unsupported encoding '" + getEncoding() + "', using default ("
					+ System.getProperty("file.encoding") + ") instead.");
		} catch (IOException e) {
			log.warn(e.getMessage(), e);
		}
	}

	private InputStream getLimitedStream(InputStream input) {
		long maxBytes = Context.get().getProperties().getInt(getTenant() + ".parser.txt.maxsize", 1024) * 1024L;
		return IOUtil.getLimitedStream(input, maxBytes);
	}
}