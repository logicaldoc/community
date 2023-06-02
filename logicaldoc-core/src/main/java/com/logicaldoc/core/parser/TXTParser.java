package com.logicaldoc.core.parser;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ibm.icu.text.CharsetDetector;
import com.ibm.icu.text.CharsetMatch;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.io.IOUtil;

/**
 * Class for parsing text (*.txt) files.
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.5
 */
public class TXTParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(TXTParser.class);

	@Override
	public String parse(File file, String filename, String encoding, Locale locale, String tenant, Document document,
			String fileVersion) {
		try (FileInputStream fis = new FileInputStream(file); BufferedInputStream bis = new BufferedInputStream(fis);) {
			return parse(bis, new ParseParameters(document, filename, fileVersion, determineEncoding(bis, encoding),
					locale, tenant));
		} catch (Exception ex) {
			log.warn("Failed to extract TXT text content", ex);
		}
		return "";
	}

	private String determineEncoding(BufferedInputStream bis, String probableEncoding) {
		if (StringUtils.isEmpty(probableEncoding)) {
			// Determine the most probable encoding
			try {
				CharsetDetector cd = new CharsetDetector();
				cd.setText(bis);
				CharsetMatch cm = cd.detect();
				if (cm != null && Charset.isSupported(cm.getName()))
					probableEncoding = cm.getName();
			} catch (Exception th) {
				log.warn("Error during TXT fileNameCharset detection", th);
			}
		}
		return probableEncoding;
	}

	@Override
	public void internalParse(InputStream input, ParseParameters parameters, StringBuilder content) {
		try {
			if (input != null)
				content.append(StringUtil.writeToString(getLimitedStream(input, parameters.getTenant()), parameters.getEncoding()));
		} catch (UnsupportedEncodingException e) {
			log.warn("Unsupported encoding '{}', using default ({}) instead.", parameters.getEncoding(),
					System.getProperty("file.encoding"));
		} catch (IOException e) {
			log.warn(e.getMessage(), e);
		}
	}

	private InputStream getLimitedStream(InputStream input, String tenant) {
		long maxBytes = Context.get().getProperties().getInt(tenant + ".parser.txt.maxsize", 1024) * 1024L;
		return IOUtil.getLimitedStream(input, maxBytes);
	}
}