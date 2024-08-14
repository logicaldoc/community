package com.logicaldoc.util.exec;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.SystemUtil;

public class ExecTest {

	protected static Logger log = LoggerFactory.getLogger(ExecTest.class);

	@Test
	public void testExecStringStringArrayFileInt() {
		String cmdPath = "C:\\LogicalDOC\\imagemagick\\convert.exe";
		File src = new File("C:\\tmp\\google.png");
		File dest = new File("C:\\tmp\\outfile.pdf");
		List<String> commandLine = List.of(cmdPath, " -compress JPEG ", src.getPath(), dest.getPath());
		try {
			int retval = new Exec().exec(commandLine, null, null, 30);
			log.info("retval: {}", retval);
			fail("Expected exception was not thrown");
		} catch (IOException e) {
			// nothing to do here
		}

		if (SystemUtil.isWindows()) {
			commandLine = List.of("target\\test-classes\\nothing.bat", "intel", "CORE i7");
			try {
				int retval = new Exec().exec(commandLine, null, null, 30);
				log.info("retval: {}", retval);
				assertEquals(0, retval);
			} catch (IOException e) {
				e.printStackTrace();
				fail("Unexpected exception was thrown");
			}
		}
	}

	@Test
	public void testIsWindows() {
		boolean xxx = new Exec().isWindows();
		boolean yyy = SystemUtil.isWindows();
		assertEquals(yyy, xxx);

		String originalOsName = System.getProperty("os.name");
		try {
			System.setProperty("os.name", "Linux");
			xxx = new Exec().isWindows();
			assertNotSame(yyy, xxx);
		} finally {
			System.setProperty("os.name", originalOsName);
		}
	}

	@Test
	public void testExec2() {
		List<String> xxx = new ArrayList<>();
		File exeFile = new File("target\\test-classes\\nothing.bat");
		xxx.add(exeFile.getPath());
		xxx.add("Siemens");
		xxx.add("Gigaset");
		xxx.add("AS410");
		try {
			int retval = new Exec().execPB(xxx, null, 10);
			log.info("retval: {}", retval);
			assertEquals(0, retval);
		} catch (IOException e) {
			e.printStackTrace();
			fail("Unexpected exception was thrown");
		}

		try {
			// Launch passing the execution folder
			File userDir = new File(System.getProperty("user.dir"));
			int retval = new Exec().execPB(xxx, userDir, 10);
			log.info("retval: {}", retval);
			assertEquals(0, retval);
		} catch (IOException e) {
			e.printStackTrace();
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testExecListOfString() {
		// nothing.bat fasfgsa < retr34.txt
		List<String> xxx = new ArrayList<>();
		xxx.add("target\\test-classes\\nothing.bat");
		xxx.add("fasfgsa");
		xxx.add("<");
		xxx.add("retr34.txt");
		try {
			int retval = new Exec().exec(xxx);
			log.info("retval: {}", retval);
			assertEquals(1, retval);
		} catch (IOException e) {
			e.printStackTrace();
			fail("Unexpected exception was thrown");
		}

		// nothing.bat fasfgsa < retr34.txt
		xxx = new ArrayList<>();
		xxx.add("target\\test-classes\\nothing.bat");
		xxx.add("Siemens");
		xxx.add("Gigaset");
		xxx.add("AS410");
		try {
			int retval = new Exec().exec(xxx);
			log.info("retval: {}", retval);
			assertEquals(0, retval);
		} catch (IOException e) {
			e.printStackTrace();
			fail("Unexpected exception was thrown");
		}

	}

	@Test
	public void testExecListOfStringStringArrayFileInt() throws IOException {

		File exeFile = new File("target\\test-classes\\nothing.bat");

		List<String> xxx = new ArrayList<>();

		xxx.add(exeFile.getPath());
		xxx.add("Lavender Haze");
		xxx.add("Maroon");
		xxx.add("Anti-Hero");

		if (SystemUtil.isWindows()) {
			try {
				File userDir = new File(System.getProperty("user.dir"));
				int retval = new Exec().exec(xxx, List.of("TS10=MidnightTS"), userDir, 30);
				log.info("retval: {}", retval);
				assertEquals(0, retval);
			} catch (IOException e) {
				e.printStackTrace();
				fail("Unexpected exception was thrown");
			}
		}
	}

	@Test
	public void testExecStringStringArrayFile() {

		File exeFile = new File("target\\test-classes\\nothing.bat @TaylorSwift #Midnights");

		try {
			if (new Exec().isWindows()) {
				String out = new Exec().execGetOutput(exeFile.getPath(), null, null);
				assertNotNull(out);
				assertTrue(out.contains("Midnights"));
			}
		} catch (IOException e) {
			e.printStackTrace();
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testExecStringStringArrayFileStringBufferInt() throws IOException {
		if (new Exec().isWindows()) {
			File userDir = new File(System.getProperty("user.dir"));
			StringBuilder sb = new StringBuilder();
			int retval = new Exec().exec("target\\test-classes\\nothing.bat @TaylorSwift #Midnights",
					List.of("TS10=MidnightTS"), userDir, sb, 20);
			assertEquals(0, retval);
			assertTrue(sb.toString().toLowerCase().contains("taylorswift"));
		}
	}

	@Test
	public void testExecStringStringArrayFileWriterInt() throws IOException {
		File exeFile = new File("target\\test-classes\\loop.bat");

		if (new Exec().isWindows()) {
			File userDir = new File(System.getProperty("user.dir"));
			StringWriter sw = new StringWriter();
			int retval = new Exec().exec(exeFile.getPath(), List.of("TS10loopcount=1000000"), userDir, sw, 5);
			log.info("retval: {}", retval);
			assertEquals(1, retval);
			String out = sw.toString();
			assertTrue(out.isEmpty() || out.contains("1000000") || out.contains("Hello World!"));
		}
	}

	@Test
	public void testNormalizePathForCommand() {
		if (new Exec().isWindows()) {
			String path = "target/test-classes/nothing.bat";
			String retval = new Exec().normalizePathForCommand(path);
			log.info("retval: {}", retval);
			String expected = "\"target\\test-classes\\nothing.bat\"";
			assertEquals(expected, retval);
		}
	}

}
