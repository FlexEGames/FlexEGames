package me.hsgamer.flexegames.api.modifier;

import net.minestom.server.instance.Instance;

/**
 * The provider for the {@link InstanceModifier}
 */
public interface InstanceModifierProvider {
    /**
     * The empty {@link InstanceModifier}
     */
    InstanceModifier EMPTY = new InstanceModifier() {
        @Override
        public void init() {
            // EMPTY
        }

        @Override
        public void clear() {
            // EMPTY
        }
    };

    /**
     * Create a new {@link InstanceModifier} for the {@link Instance}
     *
     * @param instance the instance
     * @return the modifier
     */
    default InstanceModifier getInstanceModifier(Instance instance) {
        return EMPTY;
    }
}
