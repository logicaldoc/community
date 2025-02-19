package com.logicaldoc.util.http;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import org.apache.hc.core5.http.HttpEntity;

/**
 * A HTTP response handler that writes the response into a given file
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.3
 */
public class FileHttpClientResponseHandler extends BaseHttpClientResponseHandler<String> {

	private File file;

	public FileHttpClientResponseHandler(File file) {
		super();
		this.file = file;
	}

	@Override
	public String handleEntity(HttpEntity entity) throws IOException {
		// Save response content to file on local disk
		try (BufferedInputStream bis = new BufferedInputStream(entity.getContent());
				BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(file));) {
			int inByte;
			while ((inByte = bis.read()) != -1) {
				bos.write(inByte);
			}
		}
		return null;
	}
}