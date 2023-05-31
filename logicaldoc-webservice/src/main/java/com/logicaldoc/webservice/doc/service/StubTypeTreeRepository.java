package com.logicaldoc.webservice.doc.service;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.logicaldoc.webservice.doc.model.StubTypeTree;

/**
 * 
 * @author chenjianjx
 *
 */
public class StubTypeTreeRepository {

	private Map<Class<?>, StubTypeTree> repository = new HashMap<>();

	public StubTypeTree getStubTypeTree(Class<?> type) {
		StubTypeTree stt = repository.computeIfAbsent(type, k -> {
			StubTypeTree tree = new StubTypeTree();
			tree.setType(type);
			return tree;
		});

		return stt;
	}

	public Collection<StubTypeTree> getAllTrees() {
		return repository.values();
	}

	public boolean isEmpty() {
		return repository.isEmpty();
	}

}
