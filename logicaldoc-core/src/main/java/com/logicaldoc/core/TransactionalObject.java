package com.logicaldoc.core;

/**
 * Implementations of this interface are objects that may participate into a
 * transaction
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public interface TransactionalObject {
	/**
	 * Gets the unique ID of the transaction
	 * 
	 * @return the identifier of the transaction
	 */
	public String getTransactionId();

	/**
	 * Sets the transaction's identifier
	 * 
	 * @param transactionId the identifier of the transacrion
	 */
	public void setTransactionId(String transactionId);
}
