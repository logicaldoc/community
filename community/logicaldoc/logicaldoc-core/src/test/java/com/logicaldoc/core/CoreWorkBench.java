package com.logicaldoc.core;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

import javax.mail.MessagingException;

import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailAttachment;
import com.logicaldoc.core.communication.MailUtil;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentComparator;
import com.logicaldoc.util.io.FileUtil;

public class CoreWorkBench {

	public static void main(String[] args) throws Exception {
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd");
		Document doc1 = new Document();
		doc1.setId(1L);
		doc1.setFileName("doc001.pdf");
		doc1.setDate(df.parse("2020-05-20"));

		Document doc2 = new Document();
		doc2.setId(2L);
		doc2.setFileName("doc002.pdf");
		doc2.setDate(df.parse("2020-05-15"));

		Document doc3 = new Document();
		doc3.setId(3L);
		doc3.setFileName("doc003.pdf");
		doc3.setDate(df.parse("2020-05-16"));
		
		Document doc4 = new Document();
		doc4.setId(4L);
		doc4.setFileName("doc004.pdf");
		doc4.setDate(df.parse("2020-05-21"));
		
		List<Document> docs = new ArrayList<Document>();
		docs.add(doc1);
		docs.add(doc2);
		docs.add(doc3);
		docs.add(doc4);

		Collections.sort(docs, DocumentComparator.getComparator("filename desc, date asc"));
		for (Document doc : docs) {
			System.out.println(doc.getFileName()+" > "+df.format(doc.getDate()));
		}
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

//		String inputFile = "src/test/resources/aliceDynamic.epub";
//		Reader reader = new Reader();
//		reader.setIsIncludingTextContent(true);
//		reader.setFullContent(inputFile);
//		
//		BookSection bookSection = reader.readSection(1);
//		
//		System.out.println(bookSection.getSectionContent());
//		FileUtil.writeFile(bookSection.getSectionContent(), "c:/tmp/ebook.html");

//		emailStuff();
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
