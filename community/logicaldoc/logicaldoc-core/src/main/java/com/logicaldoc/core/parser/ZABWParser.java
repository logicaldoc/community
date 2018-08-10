package com.logicaldoc.core.parser;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;
import java.util.zip.GZIPInputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Text extractor for AbiWord compressed documents.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.5
 */
public class ZABWParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(ZABWParser.class);

	@Override
	public String parse(File file, String filename, String encoding, Locale locale, String tenant) {
		String enc = encoding != null ? encoding : "UTF-8";
		FileInputStream stream = null;
		try {
			stream = new FileInputStream(file);
			GZIPInputStream gis = new GZIPInputStream(stream);
			return parse(gis, filename, enc, locale, tenant);
		} catch (Exception ex) {
			log.warn("Failed to extract Compressed AbiWord text content", ex);
		} finally {
			try {
				if (stream != null)
					stream.close();
			} catch (IOException e) {
			}
		}
		return "";
	}

	@Override
	public void internalParse(InputStream input, String filename, String encoding, Locale locale, String tenant, StringBuffer content) {
		try {
			AbiWordParser parser = new AbiWordParser();
			content.append(parser.parse(input, filename, encoding, locale, tenant));
		} catch (Exception e) {
			log.warn("Failed to extract AbiWord Compressed zabw text content", e);
		}
	}
}