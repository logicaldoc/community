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
		String commandLine = cmdPath + " -compress JPEG " + src.getPath() + " " + dest.getPath();
		try {
			int retval = new Exec().exec(commandLine, null, null, 30);
			log.info("retval: {}", retval);
			fail("Expected exception was not thrown");
		} catch (IOException e) {
			// nothing to do here
		}

		if (SystemUtil.isWindows()) {
			cmdPath = "target\\test-classes\\nothing.bat intel CORE i7";
			commandLine = cmdPath;
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
			int retval = new Exec().exec2(xxx, null, 10);
			log.info("retval: {}", retval);
			assertEquals(0, retval);
		} catch (IOException e) {
			e.printStackTrace();
			fail("Unexpected exception was thrown");
		}

		try {
			// Launch passing the execution folder
			File userDir = new File(System.getProperty("user.dir"));
			int retval = new Exec().exec2(xxx, userDir, 10);
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

		String[] envp = { "TS10=MidnightTS" };

		if (SystemUtil.isWindows()) {
			try {
				File userDir = new File(System.getProperty("user.dir"));
				int retval = new Exec().exec(xxx, envp, userDir, 30);
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
				String out = new Exec().exec(exeFile.getPath(), null, null);
				assertNotNull(out);
				assertTrue(out.contains("Midnights"));
			}
		} catch (IOException e) {
			e.printStackTrace();
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testExecStringStringArrayFileStringBufferInt() {
		File exeFile = new File("target\\test-classes\\nothing.bat @TaylorSwift #Midnights");

		String[] envp = { "TS10=MidnightTS" };

		try {
			if (new Exec().isWindows()) {
				File userDir = new File(System.getProperty("user.dir"));
				StringBuilder sb = new StringBuilder();
				int retval = new Exec().exec(exeFile.getPath(), envp, userDir, sb, 20);
				log.info("retval: {}", retval);
				assertEquals(0, retval);
				log.info("sb: {}", sb);
				String out = sb.toString();
				assertTrue(out.contains("TaylorSwift"));
			}
		} catch (IOException e) {
			e.printStackTrace();
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testExecStringStringArrayFileWriterInt() {
		File exeFile = new File("target\\test-classes\\loop.bat");

		String[] envp = { "TS10loopcount=1000000" };

		try {
			if (new Exec().isWindows()) {
				File userDir = new File(System.getProperty("user.dir"));
				StringWriter sw = new StringWriter();
				int retval = new Exec().exec(exeFile.getPath(), envp, userDir, sw, 5);
				log.info("retval: {}", retval);
				assertEquals(1, retval);
				String out = sw.toString();
				assertTrue(out.isEmpty() || out.contains("1000000") || out.contains("Hello World!"));
			}
		} catch (IOException e) {
			e.printStackTrace();
			fail("Unexpected exception was thrown");
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
