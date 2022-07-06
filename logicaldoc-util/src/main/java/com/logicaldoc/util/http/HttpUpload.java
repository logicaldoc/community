package com.logicaldoc.util.http;

import java.io.File;
import java.io.IOException;
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

	public void UploadFile() {
	}

	public static void main(String[] args) throws Exception {
		File f = new File("D:/tmp/form.gif");
		HttpUpload upload = new HttpUpload();
		upload.setFile(f);
		upload.setFileName("buf.txt");
		//upload.setURL("https://localhost:9443/servlet.gupld?new_session=true&sid=f1c65c45-73d4-45e9-aabd-8ba9ee019103");
		upload.setURL("https://demo.logicaldoc.com:443/servlet.gupld?new_session=true&sid=e6f7b4aa-d4e2-4429-b997-a77c64a67d25");
		upload.setListener(new FileBodyCounter.ProgressListener() {
			@Override
			public void transferred(long total, long increment) {
				if (total % (1024) == 0) {
					System.out.println("Transfered " + increment);
					System.out.println("Total " + total);
				}
			}
		});
		upload.upload();
	}

	public void upload() throws Exception {

		CloseableHttpResponse response = null;
		try {
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
			response = client.execute(filePost);

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
		} finally {
			if (response != null)
				try {
					response.close();
				} catch (IOException e) {
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