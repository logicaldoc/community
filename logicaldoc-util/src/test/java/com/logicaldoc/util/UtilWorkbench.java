package com.logicaldoc.util;

import java.util.Date;

import org.apache.commons.lang.StringEscapeUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document.OutputSettings;
import org.jsoup.safety.Safelist;

import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.time.DateUtil;

public class UtilWorkbench {

	/**
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		String unsafeHtmlContent="TEST<&#x003c;img src=1 onerror=confirm(document.domain)&#x003e;>TEST2";
		
		OutputSettings outputSettings = new OutputSettings().indentAmount(0).prettyPrint(false);
		Safelist whiteList = Safelist.simpleText().preserveRelativeLinks(false);
		
		String previousSanitized="";
		String sanitized = StringEscapeUtils.unescapeHtml(Jsoup.clean(unsafeHtmlContent, "", whiteList, outputSettings));
		while(!previousSanitized.equals(sanitized)) {
			previousSanitized=sanitized;
			sanitized = StringEscapeUtils.unescapeHtml(Jsoup.clean(previousSanitized, "", whiteList, outputSettings));
		}

		System.out.println(sanitized);
		
//		File file = new File("target/context.properties");
//		ContextProperties context=new ContextProperties(file);
//		context.setProperty("gui.welcome", "×ž×’×™×¢ ×œ×”×•×¤×¢×” ×‘×™×©×¨×�×œ ×•×�×ª×� × ×”× ×™×� ×ž×”×˜×‘×” ×™×™×—×•×“×™×ª");
//		// context.setProperty("gui.welcome", "pollo");
//		context.write();
//		
//		context=new ContextProperties(file);
//		System.out.println(context.getProperty("gui.welcome"));
//		System.out.println(context.getProperty("store.2.dir"));

//		Properties props=new Properties();
//		props.setProperty("gui.welcome", "×ž×’×™×¢ ×œ×”×•×¤×¢×” ×‘×™×©×¨×�×œ ×•×�×ª×� × ×”× ×™×� ×ž×”×˜×‘×” ×™×™×—×•×“×™×ª");
//		props.store(new FileOutputStream(file), null);

//		File f = new File("C:/tmp/test-split.rar");
//		System.out.println(RarUtil.listEntries(f));
//		RarUtil.extractEntry(f, "test-split.pdf", new File("pippo.pdf"));

//		P7M p7m = new P7M(new File("C:\\Users\\marco\\Downloads\\BFRI_W03 D019 LRI Pianta Tracciamento AsBuilt.pdf.p7m"));
//		p7m.read();
//		p7m.extractOriginalFile(new File("put.pdf"));

//		String command="C:\\LogicalDOC-Devel\\imagemagick\\convert.exe -compress JPEG -quality 90 -resize x200 C:\\tmp\\test.jpg C:\\tmp\\tile.jpg";
//		Exec.exec(command, null, null, 10);

//		new ZipUtil().unGZipUnTar(new File("C:\\Users\\marco\\Downloads\\GeoLite2-City_20201201.tar.gz"),
//				new File("target/ungzip"));
//		System.out.println("Unzipped");
//		FileUtils.forceDelete(new File("target/ungzip"));
//		System.out.println("Deleted");

//		System.out.println(StringUtil.printFileSize(0));
	}
}