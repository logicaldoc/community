package com.logicaldoc.core.metadata.filling;

import com.logicaldoc.core.history.History;
import com.logicaldoc.core.metadata.ExtensibleObject;

/**
 * A Filler implements its own logic to auto-fill the metadata of an
 * {@link ExtensibleObject}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.3
 */
public interface Filler {

	/**
	 * Fills an object instance
	 * 
	 * @param object the instance to fill
	 * @param content the content of the object, if not specified it will be
	 *        taken from the transaction's file.
	 * @param transaction the current transaction
	 */
	public void fill(ExtensibleObject object, String content, History transaction);
}