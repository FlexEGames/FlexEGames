package me.hsgamer.flexegames.game.pve.mob;

import me.hsgamer.minigamecore.base.Arena;
import net.minestom.server.attribute.Attribute;
import net.minestom.server.entity.EntityType;
import net.minestom.server.entity.Player;
import net.minestom.server.entity.ai.goal.MeleeAttackGoal;
import net.minestom.server.entity.ai.target.ClosestEntityTarget;
import net.minestom.server.entity.metadata.monster.zombie.ZombieMeta;
import net.minestom.server.utils.time.TimeUnit;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

public final class ZombieMob extends ArenaMob {
    public ZombieMob(Arena arena) {
        super(EntityType.ZOMBIE, arena);
        addAIGroup(
                List.of(new MeleeAttackGoal(this, 1.2, 20, TimeUnit.SERVER_TICK)),
                List.of(new ClosestEntityTarget(this, 32, Player.class::isInstance))
        );

        boolean isBaby = getStage() >= 5 && ThreadLocalRandom.current().nextBoolean();
        ((ZombieMeta) entityMeta).setBaby(isBaby);
        if (isBaby) {
            getAttribute(Attribute.MAX_HEALTH).setBaseValue(getMaxHealth() / 2);
            heal();
        }
    }
}
