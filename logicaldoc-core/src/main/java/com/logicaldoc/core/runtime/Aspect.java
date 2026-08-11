package com.logicaldoc.core.runtime;

import java.util.Arrays;

/**
 * Enumeration of relevant aspects
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public enum Aspect {
    AI, STORING, SENDINGMESSAGES, EVENTSGATHERING, SAVEHISTORY, INITIALIZATION, VALIDATION, CUSTOMID, UNIQUENESSFILENAME, WRITECHECK, SCHEDULEDTASKS, AUTOMATION, ANTIVIRUS, WORKSPACEQUOTACHECK, FORMPROCESSING, AUTONAMING, AUTOFOLDING, AUTOREVISIONING, SAVEAPICALL, WORKFLOW, AUTOFILL, UNIQUENESSCUSTOMID, USERQUOTACHECK;
    
    /**
     * Gets the right enumeration entry from the corresponding string
     * 
     * @param key the aspect's key
     * 
     * @return The corresponding entry
     */
    public static Aspect fromKey(String key) {
        return Arrays.asList(values()).stream().filter(e -> key.equalsIgnoreCase(e.key())).findFirst().orElseThrow();
    }
    
    /**
     * Converts the enumeration entry to the corresponding key in the license
     * 
     * @return The key
     */
    public String key() {
        return name().toLowerCase();
    }

    @Override
    public String toString() {
        return key();
    }
}