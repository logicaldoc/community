package com.logicaldoc.util.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.apache.commons.io.FileUtils;
import org.junit.Test;

public class FileUtilTest {

	@Test
	public void testDelete() throws IOException {
		File root = new File("target/test-destroy");

		int total = 1000;

		for (int i = 0; i < total; i++) {
			String x = Integer.toString(i);
			String result = IntStream.iterate(0, j -> j + 3).limit((int) Math.ceil(x.length() / 3.0))
					.mapToObj(j -> x.substring(j, Math.min(j + 3, x.length()))).collect(Collectors.joining("/"));
			File dir = new File(root.getPath() + "/" + result);
			dir.mkdirs();
			dir.mkdir();

			FileUtil.copyResource("/kofax.rar", new File(root.getPath() + "/1/doc/1.0"));
		}

		assertEquals(total, root.listFiles().length);

		for (File dir : root.listFiles()) {
			FileUtil.delete(dir);
		}

		assertEquals(0, root.listFiles().length);
	}

	@Test
	public void testWriteFile() throws IOException {
		File root = new File("target/test-writefile");
		FileUtils.forceMkdir(root);
		int total = 1000;

		for (int i = 0; i < total; i++) {
			File outFile =  new File(root+"/"+Integer.toString(i));
			FileUtil.writeFile(this.getClass().getResourceAsStream("/kofax.rar") , outFile.getPath() );
			assertTrue(outFile.length() > 0);
			assertEquals(new File("src/test/resources/kofax.rar").length(), outFile.length());
		}

		assertEquals(total, root.listFiles().length);

		for (File file : root.listFiles()) {
			FileUtil.delete(file);
		}

		assertEquals(0, root.listFiles().length);
	}

	@Test
	public void testMatch() throws IOException {
		assertTrue(FileUtil.matches("ReleaseNotes.txt", "*.doc,*.txt", ""));
	}

	@Test
	public void testGetName() throws IOException {
		String file = "/abc/def/pollo:test.pdf";
		assertEquals("pollo:test.pdf", FileUtil.getName(file));
	}

	@Test
	public void testGetBaseName() throws IOException {
		String file = "/abc/def/pollo:test.pdf";
		assertEquals("pollo:test", FileUtil.getBaseName(file));
	}

	@Test
	public void testGetExtension() throws IOException {
		String file = "/abc/def/pollo:test.pdf";
		assertEquals("pdf", FileUtil.getExtension(file));
	}

	@Test
	public void testGetPath() throws IOException {
		String file = "/abc/def/pollo:test.pdf";
		assertEquals("abc/def/", FileUtil.getPath(file));
	}

	@Test
	public void testReplaceInFile() throws IOException {
		File file = new File("target/test.txt");
		FileUtil.copyResource("/allowed-commands.txt", file);
		FileUtil.replaceInFile(file.getPath(), "test-classes", "XYZ");
		String content = FileUtil.readFile(file);
		assertTrue(content.contains("XYZ"));
		assertFalse(content.contains("test-classes"));
	}

	@Test
	public void testMerge() throws IOException {
		File merged = new File("target/merged.txt");
		final File file1 = new File("src/test/resources/context.properties");
		final File file2 = new File("src/test/resources/context-override.properties");
		try {
			long lines1 = FileUtil.countLines(file1);
			long lines2 = FileUtil.countLines(file2);
			FileUtil.merge(file1, file2, merged);
			assertTrue(merged.length() > 0);
			assertTrue(FileUtil.readFile(merged).contains("newprop"));
			assertEquals(lines1 + lines2 - 1, FileUtil.countLines(merged));
		} finally {
			FileUtil.delete(merged);
		}

		final List<File> mergedFiles = List.of(file1, file2, new File("src/test/resources/sql1.sql"));
		FileUtil.merge(mergedFiles, merged);
		try {
			assertTrue(merged.length() > 0);
			assertTrue(FileUtil.readFile(merged).contains("newprop"));
			assertEquals(mergedFiles.stream().collect(Collectors.summarizingLong(f -> {
				try {
					return FileUtil.countLines(f);
				} catch (IOException e) {
					return 0L;
				}
			})).getSum() - mergedFiles.size() + 1, FileUtil.countLines(merged));
		} finally {
			FileUtil.delete(merged);
		}
	}

	@Test
	public void testSplit() throws IOException {
		List<File> chunks = null;

		try {
			final File source = new File("src/test/resources/log.xml");
			final long chunkSize = 50L;
			chunks = FileUtil.split(source, chunkSize, new File("target"));
			assertEquals(Math.ceil((double) source.length() / (double) chunkSize), chunks.size(), 0);
			for (File chunk : chunks.subList(0, chunks.size() - 2))
				assertEquals(chunkSize, chunk.length());
			assertEquals(source.length() - (chunkSize * (chunks.size() - 1)), chunks.get(chunks.size() - 1).length());
		} finally {
			for (File chunk : chunks)
				FileUtil.delete(chunk);
		}

		try {
			final File source = new File("src/test/resources/log.xml");
			final long chunkSize = 10L * 1024L; // 10 KB
			chunks = FileUtil.split(source, chunkSize, new File("target"));
			assertEquals(Math.ceil((double) source.length() / (double) chunkSize), chunks.size(), 0);
			for (File chunk : chunks.subList(0, chunks.size() - 2))
				assertEquals(chunkSize, chunk.length());
			assertEquals(source.length() - (chunkSize * (chunks.size() - 1)), chunks.get(chunks.size() - 1).length());
		} finally {
			for (File chunk : chunks)
				FileUtil.delete(chunk);
		}
	}

	@Test
	public void testAppend() throws IOException {
		File outFile = new File("target/testappend.txt");
		try {
			FileUtil.writeFile(this.getClass().getResourceAsStream("/context.properties"), outFile.getPath());
			assertTrue(outFile.length() > 0);
			assertFalse(FileUtil.readFile(outFile).endsWith("pippo"));

			FileUtil.appendFile("pippo", outFile.getPath());
			assertTrue(FileUtil.readFile(outFile).endsWith("pippo"));
		} finally {
			FileUtil.delete(outFile);
		}
	}

	@Test
	public void testWriteUTF8() throws IOException {
		File outFile = new File("target/testappend.txt");
		try {
			FileUtil.writeFile(this.getClass().getResourceAsStream("/context.properties"), outFile.getPath());
			assertTrue(outFile.length() > 0);
			assertFalse(FileUtil.readFile(outFile).endsWith("pippo"));

			FileUtil.writeUTF8("pippo", outFile, true);
			assertTrue(FileUtil.readFile(outFile).endsWith("pippo"));
		} finally {
			FileUtil.delete(outFile);
		}
	}

	@Test
	public void testComputeDigest() throws IOException {
		assertEquals("06bf55447a0ec5980c16810b757a5cf5de37e70e",
				FileUtil.computeDigest(new File("src/test/resources/kofax.tar")));
		assertEquals("b65e1c92aeb3534eb5430263bf654f102227f38c",
				FileUtil.computeDigest(FileUtil.readFile(new File("src/test/resources/kofax.tar"))));
		assertNull(FileUtil.computeDigest((InputStream) null));
	}

	@Test
	public void testToByteArray() throws IOException {
		final File file = new File("src/test/resources/context.properties");
		byte[] bytes = FileUtil.toByteArray(file);
		assertEquals(file.length(), bytes.length);

		bytes = FileUtil.toByteArray(file, 0, 40);
		assertEquals(40, bytes.length);

		bytes = FileUtil.toByteArray(file, 0, file.length());
	}

	@Test
	public void testGetFolderSize() throws IOException {
		File outFolder = new File("target/folder");
		try {
			outFolder.mkdir();
			File subfolder = new File(outFolder, "subfolder");
			subfolder.mkdir();
			File file1 = new File("src/test/resources/context.properties");
			FileUtil.copy(file1, new File(outFolder, "context.properties"), 0);
			File file2 = new File("src/test/resources/context-override.properties");
			FileUtil.copy(file2, new File(outFolder, "context-overide.properties"), 0);
			FileUtil.copy(file2, new File(subfolder, "context-overide.properties"), 0);
			assertEquals(file1.length() + file2.length() + file2.length(), FileUtil.getFolderSize(outFolder));
		} finally {
			FileUtil.delete(outFolder);
		}
	}

	@Test
	public void testMoveQuitely() throws IOException {
		File outFolder = new File("target/folder");
		try {
			outFolder.mkdir();
			File subfolder = new File(outFolder, "subfolder");
			subfolder.mkdir();

			assertTrue(FileUtil.isDirEmpty(subfolder.toPath()));

			final File source = new File(outFolder, "context.properties");
			FileUtil.copy(new File("src/test/resources/context.properties"), source, 0);
			assertTrue(source.exists());
			FileUtil.isInsideFolder(outFolder, source);

			final File target = new File(subfolder, "context.properties");
			FileUtil.moveQuitely(source, target);
			assertFalse(source.exists());
			assertTrue(target.exists());
		} finally {
			FileUtil.delete(outFolder);
		}
	}

	@Test
	public void testCreateTempFile() throws IOException {
		File tempFile = null;
		File tempFolder = null;
		try {
			tempFile = FileUtil.createTempFile("pippo", "pluto");
			assertTrue(tempFile.exists());
			assertTrue(FileUtil.getName(tempFile.getName()).startsWith("pippo"));
			assertTrue(FileUtil.getName(tempFile.getName()).endsWith("pluto"));

			tempFolder = FileUtil.createTempDirectory("pippo");
			assertTrue(tempFolder.exists());
			assertTrue(FileUtil.getName(tempFolder.getName()).startsWith("pippo"));
		} finally {
			FileUtil.delete(tempFile);
			FileUtil.delete(tempFolder);
		}
	}

	@Test
	public void testGetDisplaySize() {
		assertEquals("12 Bytes", FileUtil.getDisplaySize(12L, "en"));
		assertEquals("2.5 KB", FileUtil.getDisplaySize(2598L, "en"));
		assertEquals("5.4 MB", FileUtil.getDisplaySize(5682598L, "en"));
		assertEquals("5,292,332.6 GB", FileUtil.getDisplaySize(5682598909897798L, null));
		assertEquals("5,549.4 KB", FileUtil.getDisplaySizeKB(5682598L, null));
		assertEquals(FileUtil.getDisplaySizeKB(5682598L, null), FileUtil.getDisplaySizeKB(5682598L, "en"));
	}
}