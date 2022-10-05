package com.logicaldoc.core.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;

/**
 * Utility method for using GhostScript
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.1
 */
public class GhostUtil {
	protected static Logger log = LoggerFactory.getLogger(GhostUtil.class);

	/**
	 * Prints a PDF file into a Jpeg image using 150dpi resolution
	 * 
	 * @param srcPdf The original file
	 * @param dst The output image(in case of multiple page more files are
	 *        created named dstName-xxx.dstExtension)
	 * @param page The page to print or null to print all the pages
	 * @param dpi the resolution(e.g. 75, 150, 300)
	 * 
	 * @return list of page files
	 */
	public static List<File> print(File srcPdf, File dst, Integer page, Integer dpi) {
		ContextProperties config = Context.get().getProperties();
		String ghostCommand = config.getProperty("converter.GhostscriptConverter.path");

		List<File> pages = new ArrayList<File>();
		String[] cmd = null;
		if (page != null) {
			if ("png".equals(FileUtil.getExtension(dst.getName().toLowerCase())))
				cmd = new String[] { ghostCommand, "-q", "-sDEVICE=png16m", "-dBATCH", "-dNOPAUSE",
						"-dFirstPage=" + page, "-dLastPage=" + page, "-r" + dpi, "-sOutputFile=" + dst.getPath(),
						srcPdf.getPath() };
			else
				cmd = new String[] { ghostCommand, "-q", "-sDEVICE=jpeg", "-dJPEGQ=100", "-dQFactor=1", "-dBATCH",
						"-dNOPAUSE", "-dFirstPage=" + page, "-dLastPage=" + page, "-r" + dpi,
						"-sOutputFile=" + dst.getPath(), srcPdf.getPath() };
			pages.add(dst);
		} else {
			if ("png".equals(FileUtil.getExtension(dst.getName().toLowerCase())))
				cmd = new String[] { ghostCommand, "-q", "-sDEVICE=png16m", "-dBATCH", "-dNOPAUSE", "-r" + dpi,
						"-sOutputFile=" + dst.getParent() + "/" + FilenameUtils.getBaseName(dst.getName()) + "-%04d."
								+ FileUtil.getExtension(dst.getName()),
						srcPdf.getPath() };
			else
				cmd = new String[] { ghostCommand, "-q", "-sDEVICE=jpeg", "-dJPEGQ=100", "-dQFactor=1", "-dBATCH",
						"-dNOPAUSE", "-r" + dpi,
						"-sOutputFile=" + dst.getParent() + "/" + FilenameUtils.getBaseName(dst.getName()) + "-%04d."
								+ FileUtil.getExtension(dst.getName()),
						srcPdf.getPath() };
		}

		log.debug("Executing: {}", ghostCommand);

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

			worker.join(config.getInt("converter.GhostscriptConverter.timeout", 30) * 1000L);
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

		if (page == null) {
			File root = dst.getParentFile();
			File[] children = root.listFiles(new FilenameFilter() {

				@Override
				public boolean accept(File dir, String name) {
					return name.startsWith(FilenameUtils.getBaseName(dst.getName()));
				}
			});
			pages.addAll(Arrays.asList(children));
		}

		return pages.stream().sorted().collect(Collectors.toList());
	}

	/**
	 * Prints a PDF file into a Jpeg image using 150dpi resolution
	 * 
	 * @param srcPdf The original file
	 * @param dst The output image(in case of multiple page more files are
	 *        created named dstName-xxx.dstExtension)
	 * @param page The page to print or null to print all the pages.
	 * 
	 * @return list of page files
	 */
	public static List<File> print(File srcPdf, File dst, Integer page) {
		return print(srcPdf, dst, page, 150);
	}

	/**
	 * When Runtime.exec() won't.
	 * http://www.javaworld.com/javaworld/jw-12-2000/jw-1229-traps.html
	 */
	protected static class StreamGobbler extends Thread {

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
				Thread.currentThread().interrupt();
			}
		}

		public Integer getExit() {
			return exit;
		}
	}
}