package com.logicaldoc.gui.common.client;

/**
 * An implemantation of callback that does nothing to handle the successful call
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 *
 * @param <T> The type of the return value that was declared in the synchronous
 *        version of the service method.
 */
public class EmptyAsyncCallback<T> extends DefaultAsyncCallback<T> {

	@Override
	protected void handleSuccess(T result) {
		// Do nothing
	}
}