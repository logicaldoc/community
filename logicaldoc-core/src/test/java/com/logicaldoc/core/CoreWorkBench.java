package com.logicaldoc.core;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.management.ManagementFactory;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

import javax.mail.MessagingException;
import javax.mail.internet.AddressException;
import javax.management.MBeanServerConnection;

import org.apache.http.Consts;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.message.BasicNameValuePair;

import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.communication.MailUtil;
import com.logicaldoc.util.http.HttpUtil;
import com.logicaldoc.util.io.FileUtil;
import com.sun.management.OperatingSystemMXBean;
import com.talanlabs.avatargenerator.Avatar;
import com.talanlabs.avatargenerator.IdenticonAvatar;

public class CoreWorkBench {

	/**
	 * Test sending e-mail with attachments
	 * @throws IOException 
	 */
	public static void main(String[] args) throws IOException {
//		OperatingSystemMXBean osBean = ManagementFactory.getPlatformMXBean(OperatingSystemMXBean.class);
//		// What % CPU load this current JVM is taking, from 0.0-1.0
//		System.out.println(osBean.getProcessCpuLoad());
//		
//		System.out.println(osBean.getProcessCpuTime());

	
		MBeanServerConnection mbsc = ManagementFactory.getPlatformMBeanServer();

		OperatingSystemMXBean osMBean = ManagementFactory.newPlatformMXBeanProxy(
		mbsc, ManagementFactory.OPERATING_SYSTEM_MXBEAN_NAME, OperatingSystemMXBean.class);

		long nanoBefore = System.nanoTime();
		long cpuBefore = osMBean.getProcessCpuTime();

		// Call an expensive task, or sleep if you are monitoring a remote process

		long cpuAfter = osMBean.getProcessCpuTime();
		long nanoAfter = System.nanoTime();

		long percent;
		if (nanoAfter > nanoBefore)
		 percent = ((cpuAfter-cpuBefore)*100L)/
		   (nanoAfter-nanoBefore);
		else percent = 0;

		System.out.println("Cpu usage: "+percent+"%");
	}

	private static void avatarStuff() {
		Avatar avatar = IdenticonAvatar.newAvatarBuilder().size(128, 128).build();
		BufferedImage image = avatar.create(-1050);
		System.out.println(image);
	}

	static void statsStuff() throws ClientProtocolException, IOException {
		List<NameValuePair> postParams = new ArrayList<>();

		// Add all statistics as parameters
		postParams.add(new BasicNameValuePair("id", "pippo"));
		postParams.add(new BasicNameValuePair("userno", "pluto"));
//		postParams.add(new BasicNameValuePair("sid", "paperino"));

		postParams.add(new BasicNameValuePair("product_release", "8.6"));
		postParams.add(new BasicNameValuePair("email", ""));
		postParams.add(new BasicNameValuePair("product", "LogicalDOC"));
		postParams.add(new BasicNameValuePair("product_name", "LogicalDOC Enterprise"));

//		postParams.add(new BasicNameValuePair("java_version", javaversion != null ? javaversion : ""));
//		postParams.add(new BasicNameValuePair("java_vendor", javavendor != null ? javavendor : ""));
//		postParams.add(new BasicNameValuePair("java_arch", javaarch != null ? javaarch : ""));
//		postParams.add(new BasicNameValuePair("dbms", "mysql"));

//		postParams.add(new BasicNameValuePair("os_name", osname != null ? osname : ""));
//		postParams.add(new BasicNameValuePair("os_version", osversion != null ? osversion : ""));
//		postParams.add(new BasicNameValuePair("file_encoding", fileencoding != null ? fileencoding : ""));

		postParams.add(new BasicNameValuePair("user_language", "en"));
		postParams.add(new BasicNameValuePair("user_country", "us"));

		// Sizing
		postParams.add(new BasicNameValuePair("users", "20"));
		postParams.add(new BasicNameValuePair("guests", "50"));
		postParams.add(new BasicNameValuePair("groups", "50"));
		postParams.add(new BasicNameValuePair("docs", "50"));
		postParams.add(new BasicNameValuePair("archived_docs", "50"));
		postParams.add(new BasicNameValuePair("folders", "50"));
		postParams.add(new BasicNameValuePair("tags", "50"));
		postParams.add(new BasicNameValuePair("versions", "50"));
		postParams.add(new BasicNameValuePair("histories", "50"));
		postParams.add(new BasicNameValuePair("user_histories", "50"));
		postParams.add(new BasicNameValuePair("votes", "50"));

		SimpleDateFormat isoDf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");

		/*
		 * Quotas
		 */
		postParams.add(new BasicNameValuePair("docdir", "50"));
		postParams.add(new BasicNameValuePair("indexdir", "50"));
		postParams.add(new BasicNameValuePair("quota", "50"));

//		/*
//		 * Registration
//		 */
//		postParams.add(new BasicNameValuePair("reg_name", regName != null ? regName : ""));
//		postParams.add(new BasicNameValuePair("reg_email", regEmail != null ? regEmail : ""));
//		postParams.add(new BasicNameValuePair("reg_organization", regOrganization != null ? regOrganization : ""));
//		postParams.add(new BasicNameValuePair("reg_website", regWebsite != null ? regWebsite : ""));

		HttpPost post = new HttpPost("http://stat.logicaldoc.com/stats/collect");
		UrlEncodedFormEntity entity = new UrlEncodedFormEntity(postParams, Consts.UTF_8);
		post.setEntity(entity);

		CloseableHttpClient httpclient = HttpUtil.getNotValidatingClient(60);

		// Execute request
		try (CloseableHttpResponse response = httpclient.execute(post)) {
			int responseStatusCode = response.getStatusLine().getStatusCode();
			// log status code
			if (responseStatusCode != 200)
				throw new IOException(HttpUtil.getBodyString(response));
		}
	}

	static void emailStuff() throws MessagingException, IOException {
		EMail email = MailUtil.messageToMail(CoreWorkBench.class.getResourceAsStream("/GENNAIO2020.eml"), true);
		Map<Integer, EMailAttachment> attachments = email.getAttachments();
		for (Integer index : attachments.keySet()) {
			EMailAttachment attachment = attachments.get(index);
			if (attachment.parseContent().toLowerCase().contains("compensi erogati nel mese")) {
				System.out.println(attachment.getFileName());
				System.out.println(attachment.parseContent());
			}

		}
	}

	static class Store implements Callable<Long> {
		private File file;

		private File copy;

		public Store(File file, File copy) {
			this.file = file;
			this.copy = copy;
		}

		@Override
		public Long call() throws Exception {
			FileUtil.writeFile("ciccio", file.getPath());
			System.out.println("Created file " + file.getPath());
			FileUtil.copyFile(file, copy);

			// copyFileUsingJava7Files(file, copy);
			// System.out.println("Copied file " + copy.getPath());
			FileUtil.strongDelete(file);
			FileUtil.strongDelete(copy);
			return 0L;
		}
	}

	private static void copyFileUsingStream(File source, File dest) throws IOException {
		InputStream is = null;
		OutputStream os = null;
		try {
			is = new FileInputStream(source);
			os = new FileOutputStream(dest);
			byte[] buffer = new byte[1024];
			int length;
			while ((length = is.read(buffer)) > 0) {
				os.write(buffer, 0, length);
			}
		} finally {
			is.close();
			os.close();
		}
	}

	private static void copyFileUsingChannel(File source, File dest) throws IOException {
		FileChannel sourceChannel = null;
		FileChannel destChannel = null;
		try {
			sourceChannel = new FileInputStream(source).getChannel();
			destChannel = new FileOutputStream(dest).getChannel();
			destChannel.transferFrom(sourceChannel, 0, sourceChannel.size());
		} finally {
			sourceChannel.close();
			destChannel.close();
		}
	}

	private static void copyFileUsingJava7Files(File source, File dest) throws IOException {
		Files.copy(source.toPath(), dest.toPath());
	}
}
