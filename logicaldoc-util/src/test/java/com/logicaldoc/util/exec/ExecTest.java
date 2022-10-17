package com.logicaldoc.util.exec;

import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;

import org.junit.Test;

import com.logicaldoc.util.SystemUtil;

public class ExecTest {

	@Test
	public void testExecStringStringArrayFileInt() {		
		String cmdPath = "C:\\LogicalDOC\\imagemagick\\convert.exe";
		File src = new File("C:\\tmp\\google.png");
		File dest = new File("C:\\tmp\\outfile.pdf");		
		String commandLine = cmdPath + " -compress JPEG " + src.getPath() + " " + dest.getPath();
		try {
			int retval = Exec.exec(commandLine, null, null, 30);
			System.err.println("retval: " +retval);
			fail("Expected exception was not thrown");
		} catch (IOException e) {
			// nothing to do here
		}	

		
		if (SystemUtil.isWindows()) {
			cmdPath = "target\\test-classes\\nothing.bat intel CORE i7";		
			commandLine = cmdPath;
			try {
				int retval = Exec.exec(commandLine, null, null, 30);
				System.err.println("retval: " +retval);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

}
