package com.logicaldoc.core;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

import org.apache.hc.client5.http.ClientProtocolException;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.entity.UrlEncodedFormEntity;
import org.apache.hc.client5.http.impl.classic.BasicHttpClientResponseHandler;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.core5.http.NameValuePair;
import org.apache.hc.core5.http.message.BasicNameValuePair;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.filter.AnnotationTypeFilter;

import com.logicaldoc.core.automation.AutomationDictionary;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.communication.MailUtil;
import com.logicaldoc.util.http.HttpUtil;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ResourceUtil;
import com.talanlabs.avatargenerator.Avatar;
import com.talanlabs.avatargenerator.IdenticonAvatar;

import jakarta.mail.MessagingException;

public class CoreWorkbench {

	/**
	 * Test sending e-mail with attachments
	 * 
	 * @throws IOException
	 * @throws ParseException
	 */
	public static void main(String[] args) throws IOException, ParseException {
		ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
		scanner.addIncludeFilter(new AnnotationTypeFilter(AutomationDictionary.class));
		for (BeanDefinition bd : scanner.findCandidateComponents("com.logicaldoc")) {
			if (bd.isAbstract())
				continue;
			System.out.println(bd.getBeanClassName());
		}

//		OperatingSystemMXBean osBean = ManagementFactory.getPlatformMXBean(OperatingSystemMXBean.class);
//		// What % CPU load this current JVM is taking, from 0.0-1.0
//		System.out.println(osBean.getProcessCpuLoad());
//		
//		System.out.println(osBean.getProcessCpuTime());
//
//		MBeanServerConnection mbsc = ManagementFactory.getPlatformMBeanServer();
//
//		OperatingSystemMXBean osMBean = ManagementFactory.newPlatformMXBeanProxy(mbsc,
//				ManagementFactory.OPERATING_SYSTEM_MXBEAN_NAME, OperatingSystemMXBean.class);
//
//		long nanoBefore = System.nanoTime();
//		long cpuBefore = osMBean.getProcessCpuTime();
//
//		// Call an expensive task, or sleep if you are monitoring a remote
//		// process
//
//		long cpuAfter = osMBean.getProcessCpuTime();
//		long nanoAfter = System.nanoTime();
//
//		long percent;
//		if (nanoAfter > nanoBefore)
//			percent = ((cpuAfter - cpuBefore) * 100L) / (nanoAfter - nanoBefore);
//		else
//			percent = 0;
//
//		System.out.println("Cpu usage: " + percent + "%");

//		statsStuff();
	}

	private static void avatarStuff() {
		Avatar avatar = IdenticonAvatar.newAvatarBuilder().size(128, 128).build();
		BufferedImage image = avatar.create(-1050);
		System.out.println(image);
	}

	private static void statsStuff() throws ClientProtocolException, IOException {
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
		UrlEncodedFormEntity entity = new UrlEncodedFormEntity(postParams, StandardCharsets.UTF_8);
		post.setEntity(entity);

		try (CloseableHttpClient httpClient = HttpUtil.getNotValidatingClient(60)) {
			httpClient.execute(post, new BasicHttpClientResponseHandler());
		}

		System.out.println("stats has been sent");
	}

	static void emailStuff() throws MessagingException, IOException {
		EMail email = MailUtil.messageToMail(ResourceUtil.getInputStream("GENNAIO2020.eml"), true);
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
			FileUtil.delete(file);
			FileUtil.delete(copy);
			return 0L;
		}
	}
}
