package com.logicaldoc.util.exec;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
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
			int retval = Exec.exec(commandLine, null, null, 30);
			log.info("retval: {}", retval);
			fail("Expected exception was not thrown");
		} catch (IOException e) {
			// nothing to do here
		}	

		
		if (SystemUtil.isWindows()) {
			cmdPath = "target\\test-classes\\nothing.bat intel CORE i7";		
			commandLine = cmdPath;
			try {
				int retval = Exec.exec(commandLine, null, null, 30);
				log.info("retval: {}", retval);
				assertEquals(0, retval);
			} catch (IOException e) {
				fail("Unexpected exception was thrown");
			}
		}
	}
	
	@Test
	public void testIsWindows() {
		boolean xxx = Exec.isWindows();
		boolean yyy = SystemUtil.isWindows();
		assertEquals(xxx, yyy);		
	}
	
	@Test
	public void testExec2() {
		List<String> xxx = new ArrayList<String>();
		File exeFile = new File("target\\test-classes\\nothing.bat");
		xxx.add(exeFile.getPath());
		xxx.add("Siemens");
		xxx.add("Gigaset");
		xxx.add("AS410");
		try {
			int retval = Exec.exec2(xxx, null, 10);
			log.info("retval: {}", retval);
			assertEquals(0, retval);
		} catch (IOException e) {
			fail("Unexpected exception was thrown");
		}
		
		try {
			// Launch passing the execution folder
			File userDir = new File(System.getProperty("user.dir"));
			int retval = Exec.exec2(xxx, userDir, 10);
			log.info("retval: {}", retval);
			assertEquals(0, retval);
		} catch (IOException e) {
			fail("Unexpected exception was thrown");
		}		
	}	

}
