package me.hsgamer.flexegames.api.modifier;

import net.minestom.server.instance.Instance;

public interface InstanceModifierProvider {
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

    default InstanceModifier getInstanceModifier(Instance instance) {
        return EMPTY;
    }
}
