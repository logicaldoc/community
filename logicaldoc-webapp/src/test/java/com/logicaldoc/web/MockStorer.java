package com.logicaldoc.web;

import java.io.File;
import java.io.IOException;
import java.util.List;

import com.logicaldoc.core.store.FSStorer;
import com.logicaldoc.util.Context;

public class MockStorer extends FSStorer {

	public MockStorer() {
		super();
	}

	@Override
	public int moveResourcesToStore(long docId, int targetStorageId) throws IOException {
		
		String targetRoot = Context.get().getProperties().getPropertyWithSubstitutions("store." + targetStorageId + ".dir");
		
		int moved=0;

		// List the resources
		List<String> resources = listResources(docId, null);
		for (String resource : resources) {
			File newFile = new File(targetRoot + "/" + computeRelativePath(docId) + "/" + resource);
						
			newFile.getParentFile().mkdirs();

			// Extract the original file into a temporary location
			writeToFile(docId, resource, newFile);
			moved++;
			
			// Delete the original resource
			delete(docId, resource);
		}
		
		return moved;
	}
}