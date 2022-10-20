package com.logicaldoc.util.http;

import java.io.File;
import java.util.Date;

import org.apache.http.HttpEntity;
import org.apache.http.HttpStatus;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.util.EntityUtils;

import com.logicaldoc.util.MimeType;

/**
 * 
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class HttpUpload {

	private static final int TIMEOUT = 20;

	private String url;

	private File file;

	private String fileName;

	private FileBodyCounter.ProgressListener listener;

	public void upload() throws Exception {
		HttpPost filePost = new HttpPost(url);

		String f = fileName;
		if (f == null)
			f = file.getName();

		String name = "LDOC-" + new Date().getTime();

		FileBody filePart = null;
		if (listener != null)
			filePart = new FileBodyCounter(file, ContentType.create(MimeType.getByFilename(f), "UTF-8"), name,
					listener);
		else
			filePart = new FileBody(file, ContentType.create(MimeType.getByFilename(f), "UTF-8"), name);

		HttpEntity reqEntity = MultipartEntityBuilder.create().addPart(name, filePart).build();
		filePost.setEntity(reqEntity);

		CloseableHttpClient client = HttpUtil.getNotValidatingClient(TIMEOUT);
		try (CloseableHttpResponse response = client.execute(filePost);) {

			int status = response.getStatusLine().getStatusCode();
			String respBody = "";
			HttpEntity rent = response.getEntity();
			if (rent != null)
				respBody = EntityUtils.toString(rent, "UTF-8");

			if (status == HttpStatus.SC_OK) {
				System.out.println("Upload complete, response= " + respBody);
			} else {
				String message = "Upload failed, response= " + status;
				System.out.println(message);
				throw new Exception(message);
			}
		}
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