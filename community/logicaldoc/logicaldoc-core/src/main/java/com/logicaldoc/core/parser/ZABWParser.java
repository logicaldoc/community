package com.logicaldoc.core.parser;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.GZIPInputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Text extractor for AbiWord compressed documents.
 * 
 * @author Alessandro Gasparini - Logical Objects
 * @since 3.5
 */
public class ZABWParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(ZABWParser.class);

	@Override
	public void parse(File file) {
		String enc = "UTF-8";
		FileInputStream stream = null;
		try {
			stream = new FileInputStream(file);
			GZIPInputStream gis = new GZIPInputStream(stream);
			setEncoding(enc);
			parse(gis);
		} catch (Exception ex) {
			log.warn("Failed to extract Compressed AbiWord text content", ex);
		} finally {
			try {
				if (stream != null)
					stream.close();
			} catch (IOException e) {
			}
		}
	}

	@Override
	public void internalParse(InputStream input) {
		try {
			AbiWordParser parser = new AbiWordParser();
			parser.parse(input);
			content.append(parser.getContent());
		} catch (Exception e) {
			log.warn("Failed to extract AbiWord Compressed zabw text content", e);
		}
	}
}