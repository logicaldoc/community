package com.logicaldoc.core;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

import javax.mail.MessagingException;

import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.communication.MailUtil;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ZipUtil;

public class CoreWorkBench {

	public static void main(String[] args) throws Exception {
		//
		// File xslt = new File("target/xslt");
		// FileUtils.copyURLToFile(
		// new
		// URL("https://www.fatturapa.gov.it/export/fatturazione/sdi/fatturapa/v1.2.1/fatturaPA_v1.2.1.xsl"),
		// xslt);

//		ZipUtil zipUtil = new ZipUtil();
//		List<String> entries = zipUtil.listEntries(new File("c:/tmp/ld831-index.zip"));
//		for (String entry : entries) {
//			System.out.println(entry);
//		}

		
		emailStuff();
	}

	static void emailStuff() throws MessagingException, IOException {
		EMail email = MailUtil.messageToMail(CoreWorkBench.class.getResourceAsStream("/GENNAIO2020.eml"), true);
		Map<Integer, EMailAttachment> attachments = email.getAttachments();
		for (Integer index : attachments.keySet()) {
			EMailAttachment attachment=attachments.get(index);
			if(attachment.parseContent().toLowerCase().contains("compensi erogati nel mese")) {
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
