package me.hsgamer.flexegames.api.modifier;

/**
 * The modifier for the {@link net.minestom.server.instance.Instance}
 */
public interface InstanceModifier {
    /**
     * Initialize the modifier
     */
    void init();

    /**
     * Clear the modifier
     */
    void clear();
}
