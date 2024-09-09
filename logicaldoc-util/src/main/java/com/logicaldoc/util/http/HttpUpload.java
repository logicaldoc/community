package com.logicaldoc.util.http;

import java.io.File;
import java.io.IOException;
import java.util.Date;

import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.entity.mime.FileBody;
import org.apache.hc.client5.http.entity.mime.MultipartEntityBuilder;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.core5.http.ClassicHttpResponse;
import org.apache.hc.core5.http.ContentType;
import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.HttpStatus;
import org.apache.hc.core5.http.ParseException;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.MimeType;

/**
 * Utility class to handle uploads.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class HttpUpload {

	protected static Logger log = LoggerFactory.getLogger(HttpUpload.class);

	private static final String UTF_8 = "UTF-8";

	private static final int TIMEOUT = 20;

	private String url;

	private File file;

	private String fileName;

	private FileBodyCounter.ProgressListener listener;

	public void upload() throws IOException {
		HttpPost filePost = new HttpPost(url);

		String f = fileName;
		if (f == null)
			f = file.getName();

		String name = "LDOC-" + new Date().getTime();

		FileBody filePart = null;
		if (listener != null)
			filePart = new FileBodyCounter(file, ContentType.create(MimeType.getByFilename(f), UTF_8), name, listener);
		else
			filePart = new FileBody(file, ContentType.create(MimeType.getByFilename(f), UTF_8), name);

		HttpEntity reqEntity = MultipartEntityBuilder.create().addPart(name, filePart).build();
		filePost.setEntity(reqEntity);

		CloseableHttpClient client = HttpUtil.getNotValidatingClient(TIMEOUT);
		client.execute(filePost, new StringHttpClientResponseHandler() {
			@Override
			public String handleResponse(ClassicHttpResponse response) throws IOException {
				int status = response.getCode();
				String respBody = "";
				HttpEntity rent = response.getEntity();
				if (rent != null)
					try {
						respBody = EntityUtils.toString(rent, UTF_8);
					} catch (ParseException e) {
						throw new IOException(e);
					}
				if (status == HttpStatus.SC_OK) {
					log.debug("Upload complete, response: {}", respBody);
				} else {
					log.debug("Upload failed, response: {}", status);
					throw new IOException("" + status);
				}

				return respBody;
			}
		});
	}

	public String getURL() {
		return url;
	}

	public void setURL(String url) {
		this.url = url;
	}

	public File getFile() {
		return file;
	}

	public void setFile(File file) {
		this.file = file;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public FileBodyCounter.ProgressListener getListener() {
		return listener;
	}

	public void setListener(FileBodyCounter.ProgressListener listener) {
		this.listener = listener;
	}
}