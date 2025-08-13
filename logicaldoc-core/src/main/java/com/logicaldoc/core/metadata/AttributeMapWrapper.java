package com.logicaldoc.core.metadata;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Utility class to mask a {@link Map}&lt;String,Attribute&gt; with
 * a{@link Map}&lt;String,Attribute&gt;
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 *
 */
class AttributeMapWrapper extends HashMap<String, Attribute> {

	private static final long serialVersionUID = 1L;

	private Map<String, Attribute> wrappedAttributesMap;

	public AttributeMapWrapper(Map<String, Attribute> wrappedAttributesMap) {
		super();
		this.wrappedAttributesMap = wrappedAttributesMap;
	}

	@Override
	public int size() {
		return wrappedAttributesMap.size();
	}

	@Override
	public boolean isEmpty() {
		return wrappedAttributesMap.isEmpty();
	}

	@Override
	public boolean containsKey(Object key) {
		return wrappedAttributesMap.containsKey(key);
	}

	@Override
	public boolean containsValue(Object value) {
		return wrappedAttributesMap.containsValue(value);
	}

	@Override
	public Attribute get(Object key) {
		return wrappedAttributesMap.get(key);
	}

	@Override
	public Attribute put(String key, Attribute value) {
		if (value instanceof Attribute ta)
			return wrappedAttributesMap.put(key, ta);
		else
			return null;
	}

	@Override
	public Attribute remove(Object key) {
		return wrappedAttributesMap.remove(key);
	}

	@Override
	public void putAll(Map<? extends String, ? extends Attribute> m) {
		for (Map.Entry<String, ? extends Attribute> e : m.entrySet().stream()
				.collect(Collectors.toMap(String.class::cast, v -> (Attribute) v)).entrySet()) {
			if (e.getValue() instanceof Attribute ta)
				wrappedAttributesMap.put(e.getKey(), ta);
		}
	}

	@Override
	public void clear() {
		wrappedAttributesMap.clear();
	}

	@Override
	public Set<String> keySet() {
		return wrappedAttributesMap.keySet();
	}

	@Override
	public Collection<Attribute> values() {
		return wrappedAttributesMap.values();
	}

	@Override
	public Set<Entry<String, Attribute>> entrySet() {
		return wrappedAttributesMap.entrySet().stream().map(e -> new Map.Entry<String, Attribute>() {

			@Override
			public String getKey() {
				return e.getKey();
			}

			@Override
			public Attribute getValue() {
				return e.getValue();
			}

			@Override
			public Attribute setValue(Attribute value) {
				if (value instanceof Attribute tv)
					return e.setValue(tv);
				else
					return value;
			}
		}).collect(Collectors.toSet());
	}

	@Override
	public boolean equals(Object o) {
		return wrappedAttributesMap.equals(o);
	}

	@Override
	public int hashCode() {
		return wrappedAttributesMap.hashCode();
	}

	@Override
	public Attribute getOrDefault(Object key, Attribute defaultValue) {
		Attribute ta = wrappedAttributesMap.get(key);
		if (ta != null) {
			return ta;
		} else {
			return defaultValue;
		}
	}

	@Override
	public void forEach(BiConsumer<? super String, ? super Attribute> action) {
		wrappedAttributesMap.forEach(action);
	}

	@Override
	public void replaceAll(BiFunction<? super String, ? super Attribute, ? extends Attribute> function) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Attribute putIfAbsent(String key, Attribute value) {
		if (value instanceof Attribute ta)
			return wrappedAttributesMap.putIfAbsent(key, ta);
		else
			return null;
	}

	@Override
	public boolean remove(Object key, Object value) {
		return wrappedAttributesMap.remove(key, value);
	}

	@Override
	public boolean replace(String key, Attribute oldValue, Attribute newValue) {
		if (oldValue instanceof Attribute oldTA && newValue instanceof Attribute newTA)
			return wrappedAttributesMap.replace(key, oldTA, newTA);
		else
			return false;
	}

	@Override
	public Attribute replace(String key, Attribute value) {
		if (value instanceof Attribute ta)
			return wrappedAttributesMap.replace(key, ta);
		else
			return null;
	}

	@Override
	public Attribute computeIfAbsent(String key, Function<? super String, ? extends Attribute> mappingFunction) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Attribute computeIfPresent(String key,
			BiFunction<? super String, ? super Attribute, ? extends Attribute> remappingFunction) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Attribute compute(String key,
			BiFunction<? super String, ? super Attribute, ? extends Attribute> remappingFunction) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Attribute merge(String key, Attribute value,
			BiFunction<? super Attribute, ? super Attribute, ? extends Attribute> remappingFunction) {
		throw new UnsupportedOperationException();
	}
}