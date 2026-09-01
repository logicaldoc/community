package com.logicaldoc.util.config;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.text.StrSubstitutor;
import org.apache.commons.lang3.StringUtils;

/**
 * An {@link OrderedProperties} that also adds more method to deal with properties of different type
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3.1
 *
 */
public class AdvancedProperties extends OrderedProperties {

    private static final long serialVersionUID = 1L;

    static final String BASE64_PREFIX = "_b64_";
    
    /**
     * It takes a value and expands the variables referenced in it. You can
     * reference whatever setting in the main configuration file or by using the
     * <code>env.</code> prefix you may reference whatever environment
     * variable.<br>
     * Eg., suppose a main configuration file like this:
     * 
     * <pre>
     *   default.avatar.size = 144
     * </pre>
     * 
     * And a system environment like this:
     * 
     * <pre>
     * DB_PASSWORD = abcd
     * </pre>
     * 
     * Then:
     * <ul>
     * <li>the value <code>${default.avatar.size}</code> will be expanded to
     * <code>144</code></li>
     * <li>the value <code>${env.DB_PASSWORD}</code> will be expanded to
     * <code>abcd</code></li>
     * </ul>
     * 
     * @param value The string to evaluate
     * 
     * @return The final result with variables expanded
     */
    public static String replaceVariables(String value) {
        if (value == null)
            return null;
        value = StrSubstitutor.replaceSystemProperties(value);
        if (value.contains("${env."))
            value = value.replace("${env.", "${");
        return StrSubstitutor.replace(value, System.getenv());
    }

    /**
     * Returns a setting as string. It replaces the variables by using
     * {@link ContextProperties#replaceVariables(String)}
     * 
     * @param property The name of the setting
     * 
     * @return The setting's value as string
     */
    public String getString(String property) {
        return getString(property, null);
    }

    /**
     * Returns a setting as string. It replaces the variables by using
     * {@link ContextProperties#replaceVariables(String)}
     * 
     * @param property The name of the setting
     * @param defaultValue The default value to use
     * 
     * @return The setting's value as string
     */
    public String getString(String property, String defaultValue) {
        String value = getProperty(property, defaultValue);
        if (value == null)
            return null;
    
        if (value.startsWith(BASE64_PREFIX))
            value = new String(Base64.getDecoder().decode(value.substring(BASE64_PREFIX.length())),
                    StandardCharsets.UTF_8);
        return replaceVariables(value);
    }

    public AdvancedProperties() {
        super();
    }

    public int getInt(String property) {
        return getInt(property, 0);
    }

    public int getInt(String property, int defaultValue) {
        String v = getProperty(property);
        if (v == null || v.trim().isEmpty())
            return defaultValue;
        else
            return Integer.parseInt(v.trim());
    }

    public long getLong(String property) {
        return getLong(property, 0);
    }

    public long getLong(String property, long defaultValue) {
        String v = getProperty(property);
        if (v == null || v.trim().isEmpty())
            return defaultValue;
        else
            return Long.parseLong(v.trim());
    }

    public boolean getBoolean(String property) {
        return getBoolean(property, false);
    }

    public boolean getBoolean(String property, boolean defaultValue) {
        String v = getProperty(property, Boolean.toString(defaultValue)).trim();
        return "true".equals(v) || "yes".equals(v) || "1".equals(v);
    }

    public double getDouble(String property) {
        return getDouble(property, 0D);
    }

    public double getDouble(String property, double defaultValue) {
        String v = getProperty(property);
        if (v == null || v.trim().isEmpty())
            return defaultValue;
        else
            return Double.parseDouble(v.trim());
    }

    public float getFloat(String property) {
        return getFloat(property, 0F);
    }

    public float getFloat(String property, float defaultValue) {
        String v = getProperty(property);
        if (v == null || v.trim().isEmpty())
            return defaultValue;
        else
            return Float.parseFloat(v.trim());
    }

    @Override
    public synchronized Object setProperty(String key, String value) {
        if (value.contains("\n"))
            value = BASE64_PREFIX + Base64.getEncoder().encodeToString(value.getBytes(StandardCharsets.UTF_8));
        return super.setProperty(key, value);
    }

    /**
     * Same as setProperty but the value gets encoded Base64 first
     * 
     * @param key name of the setting
     * @param value value of the setting
     * 
     * @return created value
     */
    public synchronized Object setPropertyEncoded(String key, String value) {
        return super.setProperty(key,
                BASE64_PREFIX + Base64.getEncoder().encodeToString(value.getBytes(StandardCharsets.UTF_8)));
    }

    /**
     * Returns property's value. It replaces the variables by using
     * {@link ContextProperties#replaceVariables(String)}
     * 
     * @param property The name of the property
     * 
     * @return The property's value
     */
    @Override
    public String getProperty(String property) {
        String value = super.getProperty(property);
        return replaceVariables(value);
    }

    /**
     * Returns property's value. It replaces the variables by using
     * {@link ContextProperties#replaceVariables(String)}
     * 
     * @param property The name of the setting
     * @param defaultValue The default value to use
     * 
     * @return The property's value
     */
    @Override
    public String getProperty(String property, String defaultValue) {
        String value = super.getProperty(property, defaultValue);
        return replaceVariables(value);
    }

    private String fullKey(String tenant, String property) {
        return "%s.%s".formatted(tenant, property);
    }

    public int getTenantInt(String tenant, String property) {
        return getTenantInt(tenant, property, 0);
    }

    public int getTenantInt(String tenant, String property, int defaultValue) {
        String key = fullKey(tenant, property);
        return getInt(key, defaultValue);
    }

    public long getTenantLong(String tenant, String property) {
        return getTenantLong(tenant, property, 0L);
    }

    public long getTenantLong(String tenant, String property, long defaultValue) {
        String key = fullKey(tenant, property);
        return getLong(key, defaultValue);
    }

    public boolean getTenantBoolean(String tenant, String property) {
        return getTenantBoolean(tenant, property, false);
    }

    public boolean getTenantBoolean(String tenant, String property, boolean defaultValue) {
        String key = fullKey(tenant, property);
        return getBoolean(key, defaultValue);
    }

    public String getTenantString(String tenant, String property) {
        return getTenantString(tenant, property, null);
    }

    public String getTenantString(String tenant, String property, String defaultValue) {
        String key = fullKey(tenant, property);
        return getString(key, defaultValue);
    }

    public String getTenantProperty(String tenant, String property, String defaultValue) {
        String key = fullKey(tenant, property);
        if (containsKey(key))
            return StringUtils.defaultIfEmpty(getProperty(key), defaultValue);
        else
            return getProperty(property, defaultValue);
    }

    public String getTenantProperty(String tenant, String property) {
        return getTenantProperty(tenant, property, null);
    }

    public Map<String, String> getTenantProperties(String tenant) {
        return getProperties("%s.".formatted(tenant));
    }

    /**
     * Gets all the properties whose name starts with the given prefix.
     * 
     * @param prefix property's prefix
     * 
     * @return the map property_name = property_value
     */
    public Map<String, String> getProperties(String prefix) {
        Map<String, String> props = new HashMap<>();
        for (Object key : keySet()) {
            String prop = key.toString();
            if (prop.startsWith(prefix))
                props.put(prop.substring(prefix.length()), getProperty(prop));
        }
        return props;
    }

    /**
     * Gets all the keys whose name starts with the given prefix.
     * 
     * @param prefix property's prefix
     */
    public List<String> getKeys(String prefix) {
        return getKeys().stream().filter(k -> k.startsWith(prefix)).toList();
    }

    /**
     * Removes all the properties of a specific tenant
     * 
     * @param tenant name of the tenant
     */
    public void removeTenantProperties(String tenant) {
        if ("default".equals(tenant))
            return;
        List<String> toBeDeleted = new ArrayList<>();
        for (Object key : keySet()) {
            String prop = key.toString();
            if (prop.startsWith(tenant + "."))
                toBeDeleted.add(prop);
        }
        for (String prop : toBeDeleted)
            remove(prop);
    }

    /**
     * Replicates the settings of the default tenant to a new tenant
     * 
     * @param tenant name of the tenant
     */
    public void replicateTenantSettings(String tenant) {
        Map<String, String> defaultProps = getTenantProperties("default");
        for (String prop : defaultProps.keySet()) {
            String tenantProp = tenant + "." + prop;
            if (!containsKey(tenantProp))
                setProperty(tenantProp, getProperty("default.%s".formatted(prop)));
        }
    }

}