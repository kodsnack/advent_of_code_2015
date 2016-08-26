using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AdventOfCode
{
    class Day22
    {
        static int turnID = 0;
        static int leastManaSpent = 100000;
        enum Spells { NONE, MAGIC_MISSILE, DRAIN, SHIELD, POISON, RECHARGE }
        enum Status { OK, WON, LOST, ILLEGAL }

        class Effect
        {
            public Spells spell;
            public int timer;
            public Effect(Effect e)
            {
                spell = e.spell;
                timer = e.timer;
            }
            public Effect() { }
        }

        class Turn
        {
            int turn;
            bool hardDifficulty;
            public int mana, manaSpent, playerHP, bossHP, bossDamage; 
            public Spells spell;
            public Status status;
            public List<Effect> effects;

            void CheckStatus()
            {
                if (status != Status.OK) return;

                if(bossHP <= 0)
                {
                    status = Status.WON;
                    //Console.WriteLine("Won, turn " + turn + ", mana spent: " + manaSpent);
                    if (manaSpent < leastManaSpent)
                    {
                        leastManaSpent = manaSpent;
                    }
                    
                } else if (playerHP <= 0)
                {
                    status = Status.LOST;
                }
            }

            public Turn(bool hardDifficulty = false)
            {
                this.hardDifficulty = hardDifficulty;
                status = Status.OK;
                mana = 500;
                playerHP = 50;
                bossHP = 51;
                bossDamage = 9;
                turn = 0;
                spell = Spells.NONE;
                effects = new List<Effect>();
                NextTurn();
            }

            public Turn(Turn previousTurn, Spells spell)
            {
                turnID++;
                hardDifficulty  = previousTurn.hardDifficulty;
                manaSpent       = previousTurn.manaSpent;
                status          = previousTurn.status;
                turn            = previousTurn.turn + 1;
                mana            = previousTurn.mana;
                playerHP        = previousTurn.playerHP;
                bossHP          = previousTurn.bossHP;
                bossDamage      = previousTurn.bossDamage;

                this.spell = spell;
                effects = new List<Effect>();
                foreach(var e in previousTurn.effects)
                {
                    effects.Add(new Effect(e));
                }

                if(hardDifficulty)
                {
                    playerHP -= 1;
                    CheckStatus();
                }

                // Start of player turn, effects
                for (int i = 0; i < effects.Count; i++)
                {
                    switch (effects[i].spell)
                    {
                        case Spells.POISON:
                            bossHP -= 3;
                            break;
                        case Spells.RECHARGE:
                            mana += 101;
                            break;
                    }
                    effects[i].timer--;
                }
                CheckStatus();

                // remove expired effects
                effects.RemoveAll(e => e.timer == 0);

                // cast spell
                switch (spell) {
                    case Spells.MAGIC_MISSILE:
                        mana -= 53;
                        manaSpent += 53;
                        bossHP -= 4;
                        break;
                    case Spells.DRAIN:
                        mana -= 73;
                        manaSpent += 73;
                        bossHP -= 2;
                        playerHP += 2;
                        break;
                    case Spells.SHIELD:
                        mana -= 113;
                        manaSpent += 113;
                        if(effects.Exists(e => e.spell == Spells.SHIELD)) {
                            status = Status.ILLEGAL;
                        } else
                        {
                            effects.Add(new Effect { spell = Spells.SHIELD, timer = 6 });
                        }
                        break;
                    case Spells.POISON:
                        mana -= 173;
                        manaSpent += 173;
                        if (effects.Exists(e => e.spell == Spells.POISON)) {
                            status = Status.ILLEGAL;
                        }
                        else
                        {
                            effects.Add(new Effect { spell = Spells.POISON, timer = 6 });
                        }
                        break;
                    case Spells.RECHARGE:
                        mana -= 229;
                        manaSpent += 229;
                        if (effects.Exists(e => e.spell == Spells.RECHARGE)) {
                            status = Status.ILLEGAL;
                        }
                        else
                        {
                            effects.Add(new Effect { spell = Spells.RECHARGE, timer = 5 });
                        }
                        break;
                }
                if(mana < 0)
                {
                    status = Status.ILLEGAL;
                }

                if (status == Status.ILLEGAL)
                {
                    return;
                }
                CheckStatus();

                // Boss turn
                for (int i = 0; i < effects.Count; i++)
                {
                    switch (effects[i].spell)
                    {
                        case Spells.POISON:
                            bossHP -= 3;
                            break;
                        case Spells.RECHARGE:
                            mana += 101;
                            break;
                    }
                    effects[i].timer--;
                }
                CheckStatus();

                if (effects.Exists(e => e.spell == Spells.SHIELD))
                {
                    playerHP -= Math.Max(1, bossDamage - 7);
                } else
                {
                    playerHP -= Math.Max(1, bossDamage);
                }
                // remove expired effects
                effects.RemoveAll(e => e.timer == 0);
                CheckStatus();

                if (status == Status.OK)
                {
                    NextTurn();
                }
            }

            private void NextTurn()
            {
                if (manaSpent >= leastManaSpent) return;
                new Turn(this, Spells.MAGIC_MISSILE);
                new Turn(this, Spells.DRAIN);
                new Turn(this, Spells.SHIELD);
                new Turn(this, Spells.POISON);
                new Turn(this, Spells.RECHARGE);
            }
        }

        public static void Run()
        {
            new Turn();
            Console.WriteLine("Part 1, least mana spent: " + leastManaSpent);
            leastManaSpent = 100000;
            new Turn(true);
            Console.WriteLine("Part 2, least mana spent: " + leastManaSpent);
        }
    }
}
