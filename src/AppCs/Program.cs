using System;

namespace AppCs
{
    class Program
    {
        static void Main(string[] args)
        {
            var card = Dedge.Cardizer.generateVisa();
            Console.WriteLine(card);
        }
    }
}
