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
 * Utility calass to mask a {@link Map}<String,TemplateAttribute> with
 * a{@link Map}<String,Attribute>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 *
 */
class AttributeMapWrapper extends HashMap<String, Attribute> {

	private static final long serialVersionUID = 1L;

	private Map<String, TemplateAttribute> wrappedAttributesMap;

	public AttributeMapWrapper(Map<String, TemplateAttribute> wrappedAttributesMap) {
		super();
		this.wrappedAttributesMap = wrappedAttributesMap;
	}

	public int size() {
		return wrappedAttributesMap.size();
	}

	public boolean isEmpty() {
		return wrappedAttributesMap.isEmpty();
	}

	public boolean containsKey(Object key) {
		return wrappedAttributesMap.containsKey(key);
	}

	public boolean containsValue(Object value) {
		return wrappedAttributesMap.containsValue(value);
	}

	public TemplateAttribute get(Object key) {
		return wrappedAttributesMap.get(key);
	}

	public TemplateAttribute put(String key, Attribute value) {
		if (value instanceof TemplateAttribute ta)
			return wrappedAttributesMap.put(key, ta);
		else
			return null;
	}

	public TemplateAttribute remove(Object key) {
		return wrappedAttributesMap.remove(key);
	}

	public void putAll(Map<? extends String, ? extends Attribute> m) {
		for (Map.Entry<? extends String, ? extends Attribute> e : m.entrySet()) {
			if (e.getValue() instanceof TemplateAttribute ta)
				wrappedAttributesMap.put(e.getKey().toString(), ta);
		}
	}

	public void clear() {
		wrappedAttributesMap.clear();
	}

	public Set<String> keySet() {
		return wrappedAttributesMap.keySet();
	}

	public Collection<Attribute> values() {
		return wrappedAttributesMap.values().stream().map(a -> (Attribute) a).toList();
	}

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
				if (value instanceof TemplateAttribute tv)
					return e.setValue(tv);
				else
					return value;
			}
		}).collect(Collectors.toSet());
	}

	public boolean equals(Object o) {
		return wrappedAttributesMap.equals(o);
	}

	public int hashCode() {
		return wrappedAttributesMap.hashCode();
	}

	public TemplateAttribute getOrDefault(Object key, TemplateAttribute defaultValue) {
		return wrappedAttributesMap.getOrDefault(key, defaultValue);
	}

	public void forEach(BiConsumer<? super String, ? super Attribute> action) {
		wrappedAttributesMap.forEach(action);
	}

	public void replaceAll(BiFunction<? super String, ? super Attribute, ? extends Attribute> function) {
		throw new UnsupportedOperationException();
	}

	public TemplateAttribute putIfAbsent(String key, Attribute value) {
		if (value instanceof TemplateAttribute ta)
			return wrappedAttributesMap.putIfAbsent(key, ta);
		else
			return null;
	}

	public boolean remove(Object key, Object value) {
		return wrappedAttributesMap.remove(key, value);
	}

	public boolean replace(String key, Attribute oldValue, Attribute newValue) {
		if (oldValue instanceof TemplateAttribute oldTA && newValue instanceof TemplateAttribute newTA)
			return wrappedAttributesMap.replace(key, oldTA, newTA);
		else
			return false;
	}

	public TemplateAttribute replace(String key, Attribute value) {
		if (value instanceof TemplateAttribute ta)
			return wrappedAttributesMap.replace(key, ta);
		else
			return null;
	}

	public TemplateAttribute computeIfAbsent(String key,
			Function<? super String, ? extends Attribute> mappingFunction) {
		throw new UnsupportedOperationException();
	}

	public TemplateAttribute computeIfPresent(String key,
			BiFunction<? super String, ? super TemplateAttribute, ? extends Attribute> remappingFunction) {
		throw new UnsupportedOperationException();
	}

	public TemplateAttribute compute(String key,
			BiFunction<? super String, ? super TemplateAttribute, ? extends Attribute> remappingFunction) {
		throw new UnsupportedOperationException();
	}

	public TemplateAttribute merge(String key, Attribute value,
			BiFunction<? super Attribute, ? super Attribute, ? extends Attribute> remappingFunction) {
		throw new UnsupportedOperationException();
	}
}