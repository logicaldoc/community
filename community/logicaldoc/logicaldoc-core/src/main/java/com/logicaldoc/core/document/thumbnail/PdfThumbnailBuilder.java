package com.logicaldoc.core.document.thumbnail;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.concurrent.TimeoutException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;

/**
 * This builder generates the thumbnail for a Pdf document.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class PdfThumbnailBuilder extends ImageThumbnailBuilder {
	protected static Logger log = LoggerFactory.getLogger(PdfThumbnailBuilder.class);

	@Override
	public synchronized void buildThumbnail(String sid, Document document, String fileVersion, File src, File dest,
			int size, int compression) throws IOException {

		File tmp = File.createTempFile("rendertmb", "thumb.jpg");
		try {
			renderPage(src, tmp, 1);
			super.buildThumbnail(sid, document, fileVersion, tmp, dest, size, compression);
		} catch (Throwable e) {
			throw new IOException("Thumbnail building " + e.getMessage(), e);
		} finally {
			FileUtil.strongDelete(tmp);
		}
	}

	/**
	 * When Runtime.exec() won't.
	 * http://www.javaworld.com/javaworld/jw-12-2000/jw-1229-traps.html
	 */
	class StreamGobbler extends Thread {

		InputStream is;

		StreamGobbler(InputStream is) {
			this.is = is;
		}

		@Override
		public void run() {
			try {
				InputStreamReader isr = new InputStreamReader(is);
				BufferedReader br = new BufferedReader(isr);
				String line = null;
				while ((line = br.readLine()) != null) {
					System.out.println(line);
				}
			} catch (IOException ioe) {
				log.error(ioe.getMessage());
			}
		}
	}

	protected static class Worker extends Thread {
		private final Process process;

		private Integer exit;

		public Worker(Process process) {
			this.process = process;
		}

		public void run() {
			try {
				exit = process.waitFor();
			} catch (InterruptedException ignore) {
				return;
			}
		}

		public Integer getExit() {
			return exit;
		}
	}

	protected void renderPage(File src, File dst, int page) {
		ContextProperties context = Context.get().getProperties();
		String ghostCommand = context.getProperty("command.gs");
		String[] cmd = new String[] { ghostCommand, "-q", "-sDEVICE=jpeg", "-dJPEGQ=100", "-dQFactor=1", "-dBATCH",
				"-dNOPAUSE", "-dFirstPage=" + page, "-dLastPage=" + page, "-r150", "-sOutputFile=" + dst.getPath(),
				src.getPath() };

		log.debug("Executing: " + ghostCommand);

		ProcessBuilder pb = new ProcessBuilder();
		pb.redirectErrorStream(true);
		pb.command(cmd);
		Worker worker = null;
		Process process = null;
		try {
			process = pb.start();

			StreamGobbler outputGobbler = new StreamGobbler(process.getInputStream());
			outputGobbler.start();

			worker = new Worker(process);
			worker.start();

			worker.join(20000);
			if (worker.getExit() == null)
				throw new TimeoutException();

			process.waitFor();
		} catch (IOException e) {
			log.error(e.getMessage());
		} catch (TimeoutException e) {
			log.error("Rendering timed out");
		} catch (InterruptedException ex) {
			if (worker != null)
				worker.interrupt();
			Thread.currentThread().interrupt();
		} finally {
			if (process != null)
				process.destroy();
		}
	}
}