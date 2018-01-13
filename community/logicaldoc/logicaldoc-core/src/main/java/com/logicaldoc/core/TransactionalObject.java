package com.logicaldoc.core;

/**
 * Implementations of this interface are objects that may partecipate into a
 * transaction.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.5
 * 
 */
public interface TransactionalObject {
	/**
	 * Unique ID of the transaction
	 */
	public String getTransactionId();

	public void setTransactionId(String transactionId);
}
