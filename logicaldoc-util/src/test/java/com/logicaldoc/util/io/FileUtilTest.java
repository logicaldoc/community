package com.logicaldoc.util.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.junit.Test;

public class FileUtilTest {

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
	public void testWriteFile() throws IOException {
		File outFile = new File("target/testwrite.txt");
		try {
			FileUtil.writeFile(this.getClass().getResourceAsStream("/context.properties"), outFile.getPath());
			assertTrue(outFile.length() > 0);
			assertTrue(FileUtil.readFile(outFile).contains("store.1.dir"));
			assertEquals(15, FileUtil.countLines(outFile));
		} finally {
			FileUtil.strongDelete(outFile);
		}
	}

	@Test
	public void testMerge() throws IOException {
		File merged = new File("target/merged.txt");
		try {
			FileUtil.merge(new File("src/test/resources/context.properties"),
					new File("src/test/resources/context-override.properties"), merged);
			assertTrue(merged.length() > 0);
			assertTrue(FileUtil.readFile(merged).contains("newprop"));
			assertEquals(16, FileUtil.countLines(merged));
		} finally {
			FileUtil.strongDelete(merged);
		}

		FileUtil.merge(List.of(new File("src/test/resources/context.properties"),
				new File("src/test/resources/context-override.properties"), new File("src/test/resources/sql1.sql")),
				merged);
		try {
			assertTrue(merged.length() > 0);
			assertTrue(FileUtil.readFile(merged).contains("newprop"));
			assertEquals(244, FileUtil.countLines(merged));
		} finally {
			FileUtil.strongDelete(merged);
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
				FileUtil.strongDelete(chunk);
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
				FileUtil.strongDelete(chunk);
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
			FileUtil.strongDelete(outFile);
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
			FileUtil.strongDelete(outFile);
		}
	}

	@Test
	public void testComputeDigest() throws IOException {
		assertEquals("e843fb8b615e05d33d93609360955abebe3efc88",
				FileUtil.computeDigest(new File("src/test/resources/context.properties")));
		assertEquals("e843fb8b615e05d33d93609360955abebe3efc88",
				FileUtil.computeDigest(FileUtil.readFile(new File("src/test/resources/context.properties"))));
		assertNull(FileUtil.computeDigest((InputStream) null));
	}

	@Test
	public void testToByteArray() throws IOException {
		final File file = new File("src/test/resources/context.properties");
		byte[] bytes = FileUtil.toByteArray(file);
		assertEquals(350, bytes.length);

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
			FileUtil.copy(new File("src/test/resources/context.properties"), new File(outFolder, "context.properties"),
					0);
			FileUtil.copy(new File("src/test/resources/context-override.properties"),
					new File(outFolder, "context-overide.properties"), 0);
			FileUtil.copy(new File("src/test/resources/context-override.properties"),
					new File(subfolder, "context-overide.properties"), 0);
			assertEquals(432L, FileUtil.getFolderSize(outFolder));
		} finally {
			FileUtil.strongDelete(outFolder);
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
			FileUtil.strongDelete(outFolder);
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
			FileUtil.strongDelete(tempFile);
			FileUtil.strongDelete(tempFolder);
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

	@Test
	public void testDeleteUsingOSCommand() throws IOException, InterruptedException {
		File outFile = new File("target/folder/testappend.txt");
		File outDir = new File("target/folder");
		try {
			outDir.mkdir();
			FileUtil.writeFile(this.getClass().getResourceAsStream("/context.properties"), outFile.getPath());
			assertTrue(outFile.length() > 0);
			FileUtil.deleteUsingOSCommand(outFile);
			
			while(outFile.exists())
				waiting();
			assertFalse(outFile.exists());
			
			FileUtil.deleteUsingOSCommand(outDir);
			while(outDir.exists())
				waiting();
			assertFalse(outDir.exists());
		} finally {
			FileUtil.strongDelete(outFile);
		}
	}
	
	private void waiting() throws InterruptedException {
		final int secondsToWait = 5;
		CountDownLatch lock = new CountDownLatch(1);
		lock.await(secondsToWait, TimeUnit.SECONDS);
	}
}